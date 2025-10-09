module Main (main) where

import Prelude

import Cape.Cli (Options (..), parseOptions)
import Cape.Compile (compileProgram)
import Cape.Error (MeasureError (..), exitCodeForError, renderMeasureError)
import Cape.PrettyResult (
  EvalResult (..),
  compareResult,
  evaluateCompiledCode,
  extractPrettyResult,
 )
import Cape.Tests (
  ExpectedResult (..),
  InputType (..),
  TestCase (..),
  TestInput (..),
  TestSuite (..),
  getTestBaseDir,
  isPendingTest,
  loadTestSuite,
  resolveExpectedResult,
  resolveTestInput,
 )
import Control.Exception qualified as E
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.List (maximum, minimum, (!!))
import Data.SatInt (unSatInt)
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import PlutusCore qualified as PLC
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText, renderParseError)
import PlutusCore.Data.Compact.Printer (dataToCompactText)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusCore.Size (unSize)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code (getPlc)
import System.Console.ANSI (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  hSupportsANSI,
  setSGR,
 )
import System.Exit qualified as Exit
import System.IO (putChar)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLCParser
import UntypedPlutusCore.Size (programSize)

-- | Colorize text with green for PASS results
colorizePass :: Text -> IO ()
colorizePass text = do
  supportsColor <- hSupportsANSI stdout
  if supportsColor
    then do
      setSGR [SetColor Foreground Vivid Green]
      putText text
      setSGR [Reset]
      putChar '\n'
    else putTextLn text

-- | Colorize text with red for FAIL results
colorizeFail :: Text -> IO ()
colorizeFail text = do
  supportsColor <- hSupportsANSI stdout
  if supportsColor
    then do
      setSGR [SetColor Foreground Vivid Red]
      putText text
      setSGR [Reset]
      putChar '\n'
    else putTextLn text

-- | Colorize text with yellow for PENDING results
colorizePending :: Text -> IO ()
colorizePending text = do
  supportsColor <- hSupportsANSI stdout
  if supportsColor
    then do
      setSGR [SetColor Foreground Vivid Yellow]
      putText text
      setSGR [Reset]
      putChar '\n'
    else putTextLn text

-- | Colorize text with red for PENDING PASSING results (error case)
colorizePendingPassing :: Text -> IO ()
colorizePendingPassing text = do
  supportsColor <- hSupportsANSI stdout
  if supportsColor
    then do
      setSGR [SetColor Foreground Vivid Red]
      putText text
      setSGR [Reset]
      putChar '\n'
    else putTextLn text

main :: IO ()
main = do
  Options {optInput, optOutput, optTests, optDebugContext} <-
    parseOptions
  -- Run test execution
  E.try
    ( runTestSuite
        optInput
        optTests
        (Just optOutput)
        optDebugContext
    )
    >>= \case
      Left err -> do
        putStrLn (renderMeasureError err)
        Exit.exitWith (exitCodeForError err)
      Right () -> pass

-- | Run test suite
runTestSuite :: FilePath -> FilePath -> Maybe FilePath -> Bool -> IO ()
runTestSuite uplcFile testsFile metricsFileOpt debugContext = do
  -- Load test suite
  testSuite <- loadTestSuite testsFile
  let baseDir = getTestBaseDir testsFile

  -- Read and parse the UPLC program
  uplcText <- readTextUtf8 uplcFile
  program <- case PLC.runQuote (runExceptT (UPLCParser.parseProgram uplcText)) ::
                    Either
                      (PLC.Error PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan)
                      (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan) of
    Left err -> E.throwIO (UPLCParseError uplcFile (show err))
    Right prog -> pure prog

  -- Run each test case and collect results and metrics
  testResultsAndMetrics <- forM (tsTests testSuite) \testCase -> do
    runSingleTest program baseDir testSuite testCase debugContext

  let (testResults, evaluationMetrics) = unzip testResultsAndMetrics

  -- Evaluate test suite with pending support
  evaluateTestSuite testResults

  -- Write detailed metrics file with per-evaluation data
  case metricsFileOpt of
    Just metricsFile -> writeDetailedMetrics program evaluationMetrics metricsFile
    Nothing -> putTextLn "\nValidation completed successfully (no metrics file written)"

-- | Evaluate test suite results with pending support
evaluateTestSuite :: [TestResult] -> IO ()
evaluateTestSuite results = do
  let nonPendingTests = filter (not . trPending) results
      passedNonPending = filter trPassed nonPendingTests
      pendingTests = filter trPending results
      pendingPassingTests = filter (\r -> trPending r && trPassed r) results

  putTextLn $
    "\nTest results: "
      <> show (length passedNonPending)
      <> " of "
      <> show (length nonPendingTests)
      <> " tests passed"

  when (not (null pendingTests)) $
    putTextLn $
      "Pending tests: " <> show (length pendingTests) <> " (expected failures)"

  if not (null pendingPassingTests)
    then do
      putTextLn
        "❌ Some pending tests are now passing! Please remove 'pending: true' from:"
      forM_ pendingPassingTests $ \r ->
        putTextLn $ "  - " <> trName r
      E.throwIO (VerificationError "Pending tests are passing")
    else
      if length passedNonPending == length nonPendingTests
        then
          putTextLn "✓ All non-pending tests passed!"
        else
          E.throwIO (VerificationError "Some tests failed")

-- | Run a single test case
runSingleTest ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  FilePath ->
  TestSuite ->
  TestCase ->
  Bool ->
  IO (TestResult, EvaluationMetrics)
runSingleTest program baseDir testSuite testCase debugContext = do
  (testResult, evalRes) <- case tcInput testCase of
    Nothing -> do
      -- No input - run program directly
      code <- compileProgram program Nothing
      let evalRes = evaluateCompiledCode code
      testResult <- checkTestResult evalRes testCase baseDir
      pure (testResult, evalRes)
    Just testInput -> do
      -- Apply input to validator based on input type
      inputText <- resolveTestInput baseDir testSuite testInput

      case tiType testInput of
        UPLC -> do
          -- Parse input as UPLC term and apply to program
          -- Need to wrap the term in a minimal program for parsing
          let wrappedInput = "(program 1.1.0 " <> inputText <> ")"
          inputTerm <- case PLC.runQuote (runExceptT (UPLCParser.parseProgram wrappedInput)) ::
                              Either
                                (PLC.Error PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan)
                                (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan) of
            Left err -> E.throwIO (UPLCParseError "uplc_input" (show err))
            Right inputProgram -> do
              let UPLC.Program _ _ term = inputProgram
              pure term

          -- Create applied program by applying the main program to the input term
          let UPLC.Program ann ver mainTerm = program
              appliedTerm = UPLC.Apply ann mainTerm inputTerm
              appliedProgram = UPLC.Program ann ver appliedTerm

          -- Output debug information if requested
          when debugContext $ do
            putStrLn $
              "DEBUG_CAPE["
                <> toString (tcName testCase)
                <> "]: raw_uplc input: "
                <> show inputText

          appliedCode <- compileProgram appliedProgram Nothing
          let evalRes = evaluateCompiledCode appliedCode
          testResult <- checkTestResult evalRes testCase baseDir
          pure (testResult, evalRes)
        _ -> do
          -- Handle BuiltinData and ScriptContext (existing logic)
          builtinData <- parseBuiltinDataFromText inputText

          -- Output debug information if requested
          when debugContext $ do
            let plcData = Builtins.builtinDataToData builtinData
                compactText = dataToCompactText plcData
            putStrLn $
              "DEBUG_CAPE[" <> toString (tcName testCase) <> "]: " <> show compactText

          appliedCode <- compileProgram program (Just builtinData)
          let evalRes = evaluateCompiledCode appliedCode
          testResult <- checkTestResult evalRes testCase baseDir
          pure (testResult, evalRes)

  -- Extract metrics from evaluation result
  let budget = evalResultBudget evalRes
      ExBudget {exBudgetCPU = ExCPU cpu, exBudgetMemory = ExMemory mem} = budget
      executionResult = if trPassed testResult then "success" else "error"
      description = fromMaybe "" (tcDescription testCase)

  let evaluationMetrics =
        EvaluationMetrics
          { evalName = tcName testCase
          , evalDescription = description
          , evalCpuUnits = fromIntegral (unSatInt cpu)
          , evalMemoryUnits = fromIntegral (unSatInt mem)
          , evalExecutionResult = executionResult
          }

  pure (testResult, evaluationMetrics)

-- | Parse BuiltinData from text representation
parseBuiltinDataFromText :: Text -> IO V3.BuiltinData
parseBuiltinDataFromText inputText = do
  case parseBuiltinDataText inputText of
    Left parseErr ->
      E.throwIO
        (UPLCParseError "builtin_data_input" (show (renderParseError parseErr)))
    Right builtinData ->
      pure $ V3.BuiltinData builtinData

-- | Check test result against expected outcome with pending support
checkTestResult :: EvalResult -> TestCase -> FilePath -> IO TestResult
checkTestResult evalRes testCase baseDir = do
  let testName = tcName testCase
      isPending = isPendingTest testCase

  expectedContent <- resolveExpectedResult baseDir (tcExpected testCase)
  let resultType = erType (tcExpected testCase)
      matches = compareResult evalRes resultType expectedContent

  case (isPending, matches) of
    (True, False) -> do
      colorizePending $ "⏳ PENDING " <> testName <> " (expected failure)"
      pure $ TestResult False True testName
    (True, True) -> do
      colorizePendingPassing $ "❌ PENDING PASSING " <> testName
      pure $ TestResult True True testName -- This will cause suite failure
    (False, True) -> do
      colorizePass $ "✓ PASS " <> testName
      pure $ TestResult True False testName
    (False, False) -> do
      colorizeFail $ "✗ FAIL " <> testName
      -- Show evaluation result for debugging
      case extractPrettyResult evalRes of
        Left errMsg -> putTextLn $ "    Error: " <> errMsg
        Right value -> putTextLn $ "    Actual: " <> value
      case expectedContent of
        Nothing -> putTextLn "    Expected: ERROR"
        Just expected -> putTextLn $ "    Expected: " <> expected
      pure $ TestResult False False testName

-- | Write detailed metrics with per-evaluation data and aggregations
writeDetailedMetrics ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  [EvaluationMetrics] ->
  FilePath ->
  IO ()
writeDetailedMetrics program evaluations metricsFile = do
  code <- compileProgram program Nothing
  let scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes
      termSize = fromIntegral $ unSize $ programSize $ getPlc code

  -- Extract CPU and memory data for aggregations
  let cpuData =
        [ (evalCpuUnits eval, evalExecutionResult eval == "success") | eval <- evaluations
        ]
      memoryData =
        [ (evalMemoryUnits eval, evalExecutionResult eval == "success")
        | eval <- evaluations
        ]

  -- Calculate aggregations
  let cpuAggregations = calculateBudgetAggregations cpuData
      memoryAggregations = calculateBudgetAggregations memoryData

  -- Create measurements object
  let measurements =
        Measurements
          { measCpuAggregations = cpuAggregations
          , measMemoryAggregations = memoryAggregations
          , measScriptSizeBytes = fromIntegral scriptSize
          , measTermSize = termSize
          }

  -- Get current timestamp
  currentTime <- getCurrentTime
  let timestamp = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime

  -- Create execution environment info
  let execEnv = Json.object [("evaluator", Json.String "PlutusTx.Eval-1.52.0.0")]

  -- Create complete metrics object
  let metrics =
        Metrics
          { metricsMeasurements = measurements
          , metricsEvaluations = evaluations
          , metricsScenario = "unknown" -- TODO: extract from test suite or CLI
          , metricsVersion = "1.0.0"
          , metricsExecutionEnvironment = execEnv
          , metricsTimestamp = timestamp
          , metricsNotes = Nothing
          }

  LBS.writeFile metricsFile $ AesonPretty.encodePretty metrics
  putTextLn $ "Detailed metrics written to " <> toText metricsFile

-- | Test result including pending status
data TestResult = TestResult
  { trPassed :: Bool
  , trPending :: Bool
  , trName :: Text
  }

-- | Individual evaluation metrics
data EvaluationMetrics = EvaluationMetrics
  { evalName :: Text
  , evalDescription :: Text
  , evalCpuUnits :: Integer
  , evalMemoryUnits :: Integer
  , evalExecutionResult :: Text -- "success" or "error"
  }

instance ToJSON EvaluationMetrics where
  toJSON (EvaluationMetrics name desc cpu mem result) =
    Json.object
      [ "name" .= name
      , "description" .= desc
      , "cpu_units" .= cpu
      , "memory_units" .= mem
      , "execution_result" .= result
      ]

-- | Aggregated budget measurements
data BudgetAggregations = BudgetAggregations
  { aggMaximum :: Integer
  , aggSum :: Integer
  , aggMinimum :: Integer
  , aggMedian :: Integer
  , aggSumPositive :: Integer
  , aggSumNegative :: Integer
  }

instance ToJSON BudgetAggregations where
  toJSON (BudgetAggregations maxVal sumVal minVal medVal sumPos sumNeg) =
    Json.object
      [ "maximum" .= maxVal
      , "sum" .= sumVal
      , "minimum" .= minVal
      , "median" .= medVal
      , "sum_positive" .= sumPos
      , "sum_negative" .= sumNeg
      ]

-- | Top-level measurements with aggregations
data Measurements = Measurements
  { measCpuAggregations :: BudgetAggregations
  , measMemoryAggregations :: BudgetAggregations
  , measScriptSizeBytes :: Integer
  , measTermSize :: Integer
  }

instance ToJSON Measurements where
  toJSON (Measurements cpuAgg memAgg scriptSize termSize) =
    Json.object
      [ "cpu_units" .= cpuAgg
      , "memory_units" .= memAgg
      , "script_size_bytes" .= scriptSize
      , "term_size" .= termSize
      ]

-- | Complete metrics data structure matching the schema
data Metrics = Metrics
  { metricsMeasurements :: Measurements
  , metricsEvaluations :: [EvaluationMetrics]
  , metricsScenario :: Text
  , metricsVersion :: Text
  , metricsExecutionEnvironment :: Json.Value
  , metricsTimestamp :: Text
  , metricsNotes :: Maybe Text
  }

instance ToJSON Metrics where
  toJSON (Metrics measurements evaluations scenario version execEnv timestamp notes) =
    Json.object $
      [ "measurements" .= measurements
      , "evaluations" .= evaluations
      , "scenario" .= scenario
      , "version" .= version
      , "execution_environment" .= execEnv
      , "timestamp" .= timestamp
      ]
        <> case notes of
          Nothing -> []
          Just n -> ["notes" .= n]

-- | Calculate aggregations for a list of budget values with success/error info
calculateBudgetAggregations :: [(Integer, Bool)] -> BudgetAggregations
calculateBudgetAggregations budgetData =
  let values = map fst budgetData
      successValues = [val | (val, True) <- budgetData]
      errorValues = [val | (val, False) <- budgetData]
      sortedValues = sort values
      median = calculateMedian sortedValues
   in BudgetAggregations
        { aggMaximum = if null values then 0 else maximum values
        , aggSum = sum values
        , aggMinimum = if null values then 0 else minimum values
        , aggMedian = median
        , aggSumPositive = sum successValues
        , aggSumNegative = sum errorValues
        }

-- | Calculate median from a sorted list
calculateMedian :: [Integer] -> Integer
calculateMedian [] = 0
calculateMedian xs =
  let len = length xs
      mid = len `div` 2
   in if even len
        then (xs !! (mid - 1) + xs !! mid) `div` 2
        else xs !! mid

-- Helper: strict UTF-8 file reader with exception handling
readTextUtf8 :: FilePath -> IO Text
readTextUtf8 fp = do
  bs <- readFileBS fp
  case TE.decodeUtf8' bs of
    Left _ -> E.throwIO (FileDecodeError fp)
    Right t -> pure t
