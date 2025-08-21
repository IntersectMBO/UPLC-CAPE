module Main (main) where

import Prelude

import Cape.Cli (Options (..), parseOptions)
import Cape.Compile (applyPrograms, compileProgram)
import Cape.Error (MeasureError (..), exitCodeForError, renderMeasureError)
import Cape.PrettyResult (compareResult, extractPrettyResult)
import Cape.Tests (
  ExpectedResult (..),
  InputType (..),
  TestCase (..),
  TestInput (..),
  TestSuite (..),
  getTestBaseDir,
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
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusCore.MkPlc qualified as MkPlc
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code (countAstNodes)
import PlutusTx.Eval (EvalResult (..), evaluateCompiledCode)
import System.Exit qualified as Exit
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLCParser

main :: IO ()
main = do
  Options {optInput, optOutput, optTests} <- parseOptions
  -- Run test execution
  E.try (runTestSuite optInput optTests optOutput) >>= \case
    Left err -> do
      putStrLn (renderMeasureError err)
      Exit.exitWith (exitCodeForError err)
    Right () -> pass

-- | Run test suite
runTestSuite :: FilePath -> FilePath -> FilePath -> IO ()
runTestSuite uplcFile testsFile metricsFile = do
  putTextLn $ "Running test suite: " <> toText testsFile
  putTextLn $ "Testing program: " <> toText uplcFile

  -- Load test suite
  testSuite <- loadTestSuite testsFile
  let baseDir = getTestBaseDir testsFile

  -- Read and parse the UPLC program
  uplcText <- readTextUtf8 uplcFile
  program <- case PLC.runQuote (runExceptT (UPLCParser.parseProgram uplcText)) of
    Left err -> E.throwIO (UPLCParseError uplcFile (show err))
    Right prog -> pure prog

  -- Run each test case and collect results and metrics
  testResultsAndMetrics <- forM (tsTests testSuite) \testCase -> do
    putTextLn $ "\nRunning test: " <> tcName testCase
    runSingleTest program baseDir testSuite testCase

  let (testResults, evaluationMetrics) = unzip testResultsAndMetrics

  -- Check if all tests passed

  putTextLn $
    "\nTest results: "
      <> show (length (filter id testResults))
      <> " of "
      <> show (length testResults)
      <> " tests passed"

  if and testResults
    then do
      putTextLn "All tests passed! ✓"
      -- Write detailed metrics file with per-evaluation data
      writeDetailedMetrics program evaluationMetrics metricsFile
    else do
      putTextLn "Some tests failed! ✗"
      E.throwIO (VerificationError "Test suite failed")

-- | Run a single test case
runSingleTest ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  FilePath ->
  TestSuite ->
  TestCase ->
  IO (Bool, EvaluationMetrics)
runSingleTest program baseDir testSuite testCase = do
  (testPassed, evalRes) <- case tcInput testCase of
    Nothing -> do
      -- No input - run program directly
      code <- compileProgram program
      let evalRes = evaluateCompiledCode code
      testResult <- checkTestResult evalRes (tcExpected testCase) baseDir
      pure (testResult, evalRes)
    Just testInput -> do
      -- Apply input to program
      inputText <- resolveTestInput baseDir testSuite testInput
      inputProgram <- parseInputProgram (tiType testInput) inputText
      appliedCode <- applyPrograms program inputProgram
      let evalRes = evaluateCompiledCode appliedCode
      testResult <- checkTestResult evalRes (tcExpected testCase) baseDir
      pure (testResult, evalRes)

  -- Extract metrics from evaluation result
  let budget = evalResultBudget evalRes
      ExBudget {exBudgetCPU = ExCPU cpu, exBudgetMemory = ExMemory mem} = budget
      executionResult = if testPassed then "success" else "error"
      description = fromMaybe "" (tcDescription testCase)

  let evaluationMetrics =
        EvaluationMetrics
          { evalName = tcName testCase
          , evalDescription = description
          , evalCpuUnits = fromIntegral (unSatInt cpu)
          , evalMemoryUnits = fromIntegral (unSatInt mem)
          , evalExecutionResult = executionResult
          }

  pure (testPassed, evaluationMetrics)

-- | Parse input based on type
parseInputProgram ::
  InputType ->
  Text ->
  IO (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan)
parseInputProgram inputType inputText =
  case inputType of
    BuiltinData -> do
      -- Parse BuiltinData using custom parser
      case parseBuiltinDataText inputText of
        Left parseErr ->
          E.throwIO
            (UPLCParseError "builtin_data_input" (show (renderParseError parseErr)))
        Right builtinData -> do
          -- Convert Data to UPLC constant
          let ann = PLC.SrcSpan "" 1 1 1 1
              dataConstant = MkPlc.mkConstant ann builtinData
              uplcProgram = UPLC.Program ann (UPLC.Version 1 1 0) dataConstant
          pure uplcProgram
    RawUPLC -> do
      -- Parse as raw UPLC program
      case PLC.runQuote (runExceptT (UPLCParser.parseProgram inputText)) of
        Left err -> E.throwIO (UPLCParseError "raw_uplc_input" (show err))
        Right prog -> pure prog
    ScriptContext -> do
      -- Script context input is already converted to BuiltinData text representation
      -- by resolveTestInput, so parse it as BuiltinData
      case parseBuiltinDataText inputText of
        Left parseErr ->
          E.throwIO
            (UPLCParseError "builtin_data_input" (show (renderParseError parseErr)))
        Right builtinData -> do
          -- Convert Data to UPLC constant
          let ann = PLC.SrcSpan "" 1 1 1 1
              dataConstant = MkPlc.mkConstant ann builtinData
              uplcProgram = UPLC.Program ann (UPLC.Version 1 1 0) dataConstant
          pure uplcProgram

-- | Check test result against expected outcome
checkTestResult :: EvalResult -> ExpectedResult -> FilePath -> IO Bool
checkTestResult evalRes expectedResult baseDir = do
  expectedContent <- resolveExpectedResult baseDir expectedResult
  let resultType = erType expectedResult
      matches = compareResult evalRes resultType expectedContent

  if matches
    then do
      putTextLn "  ✓ PASS"
      pure True
    else do
      putTextLn "  ✗ FAIL"
      case extractPrettyResult evalRes of
        Left errMsg -> putTextLn $ "    Actual: ERROR - " <> errMsg
        Right value -> putTextLn $ "    Actual: " <> value
      case expectedContent of
        Nothing -> putTextLn "    Expected: ERROR"
        Just expected -> putTextLn $ "    Expected: " <> expected
      pure False

-- | Write detailed metrics with per-evaluation data and aggregations
writeDetailedMetrics ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  [EvaluationMetrics] ->
  FilePath ->
  IO ()
writeDetailedMetrics program evaluations metricsFile = do
  code <- compileProgram program
  let scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes
      termSize = countAstNodes code

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
        ++ case notes of
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
