{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Prelude

import App.BuiltinDataParser (parseBuiltinDataText, renderParseError)
import App.Cli (Options (..), parseOptions)
import App.Compile (applyPrograms, compileProgram)
import App.Error (MeasureError (..), exitCodeForError, renderMeasureError)
import App.PrettyResult (compareResult, extractPrettyResult)
import App.ScriptContextBuilder (ScriptContextSpec, buildScriptContext)
import App.Tests (
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
import Control.Monad.Except (runExceptT)
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.SatInt (unSatInt)
import Data.Text.Encoding qualified as TE
import PlutusCore qualified as PLC
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
  case optTests of
    Just testsFile -> do
      -- Run unified test execution mode
      E.try (runTestSuite optInput testsFile optOutput) >>= \case
        Left err -> do
          putStrLn (renderMeasureError err)
          Exit.exitWith (exitCodeForError err)
        Right () -> pass
    Nothing -> do
      -- Run simple measurement mode (no tests)
      E.try (measureUplcFileSimple optInput optOutput) >>= \case
        Left err -> do
          putStrLn (renderMeasureError err)
          Exit.exitWith (exitCodeForError err)
        Right () -> pass

-- | Run test suite in unified test execution mode
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

  -- Run each test case
  testResults <- forM (tsTests testSuite) $ \testCase -> do
    putTextLn $ "\nRunning test: " <> tcName testCase
    runSingleTest program baseDir testCase

  -- Check if all tests passed
  let allPassed = all id testResults
      passedCount = length (filter id testResults)
      totalCount = length testResults

  putTextLn $
    "\nTest results: " <> show passedCount <> "/" <> show totalCount <> " passed"

  if allPassed
    then do
      putTextLn "All tests passed! ✓"
      -- Write a simple success metrics file for now
      writeSuccessMetrics program metricsFile
    else do
      putTextLn "Some tests failed! ✗"
      E.throwIO (VerificationError "Test suite failed")

-- | Run a single test case
runSingleTest ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  FilePath ->
  TestCase ->
  IO Bool
runSingleTest program baseDir testCase = do
  case tcInput testCase of
    Nothing -> do
      -- No input - run program directly
      code <- compileProgram program
      let evalRes = evaluateCompiledCode code
      checkTestResult evalRes (tcExpected testCase) baseDir
    Just testInput -> do
      -- Apply input to program
      inputText <- resolveTestInput baseDir testInput
      inputProgram <- parseInputProgram (tiType testInput) inputText
      appliedCode <- applyPrograms program inputProgram
      let evalRes = evaluateCompiledCode appliedCode
      checkTestResult evalRes (tcExpected testCase) baseDir

-- | Parse input based on type
parseInputProgram ::
  InputType ->
  Text ->
  IO (UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan)
parseInputProgram inputType inputText = case inputType of
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
    -- Parse ScriptContext DSL JSON
    case Json.eitherDecodeStrict (encodeUtf8 inputText) of
      Left jsonErr -> E.throwIO (UPLCParseError "script_context_input" ("JSON parse error: " <> jsonErr))
      Right (spec :: ScriptContextSpec) -> do
        case buildScriptContext spec of
          Left buildErr -> E.throwIO (UPLCParseError "script_context_input" ("ScriptContext build error: " <> Text.unpack buildErr))
          Right builtinData -> do
            -- Convert ScriptContext BuiltinData to UPLC constant
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

-- | Write simple success metrics
writeSuccessMetrics ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  FilePath ->
  IO ()
writeSuccessMetrics program metricsFile = do
  code <- compileProgram program
  let evalRes = evaluateCompiledCode code
      budget = evalResultBudget evalRes
      ExBudget {exBudgetCPU = ExCPU cpu, exBudgetMemory = ExMemory mem} = budget
      scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes

  LBS.writeFile metricsFile $
    AesonPretty.encodePretty
      Metrics
        { cpu_units = fromIntegral (unSatInt cpu)
        , memory_units = fromIntegral (unSatInt mem)
        , script_size_bytes = fromIntegral scriptSize
        , term_size = countAstNodes code
        }
  putTextLn $ "Metrics written to " <> toText metricsFile

-- | Simple UPLC measurement without tests
measureUplcFileSimple :: FilePath -> FilePath -> IO ()
measureUplcFileSimple uplcFile metricsFile = do
  putTextLn $ "Measuring UPLC program: " <> toText uplcFile

  -- Read and parse the UPLC program
  uplcText <- readTextUtf8 uplcFile
  program <- case PLC.runQuote (runExceptT (UPLCParser.parseProgram uplcText)) of
    Left err -> E.throwIO (UPLCParseError uplcFile (show err))
    Right prog -> pure prog

  -- Compile and evaluate
  code <- compileProgram program
  let evalRes = evaluateCompiledCode code
      budget = evalResultBudget evalRes
      ExBudget {exBudgetCPU = ExCPU cpu, exBudgetMemory = ExMemory mem} = budget
      scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes

  -- Check if evaluation succeeded and show result
  case extractPrettyResult evalRes of
    Left errMsg -> putTextLn $ "Program evaluation failed: " <> errMsg
    Right value -> putTextLn $ "Program evaluation succeeded with result: " <> value

  -- Write metrics regardless of evaluation result
  LBS.writeFile metricsFile $
    AesonPretty.encodePretty
      Metrics
        { cpu_units = fromIntegral (unSatInt cpu)
        , memory_units = fromIntegral (unSatInt mem)
        , script_size_bytes = fromIntegral scriptSize
        , term_size = countAstNodes code
        }

  putTextLn $ "Metrics written to " <> toText metricsFile
  putTextLn $ "Script size: " <> toText @String (show scriptSize) <> " bytes"
  putTextLn $ "CPU units: " <> toText @String (show cpu)
  putTextLn $ "Memory units: " <> toText @String (show mem)
  putTextLn $ "Term size: " <> toText @String (show (countAstNodes code))

-- | Metrics data structure matching the schema
data Metrics = Metrics
  { cpu_units :: Integer
  , memory_units :: Integer
  , script_size_bytes :: Integer
  , term_size :: Integer
  }

instance ToJSON Metrics where
  toJSON (Metrics cpu mem scriptSize termSize) =
    Json.object
      [ "cpu_units" .= cpu
      , "memory_units" .= mem
      , "script_size_bytes" .= scriptSize
      , "term_size" .= termSize
      ]

-- Helper: strict UTF-8 file reader with exception handling
readTextUtf8 :: FilePath -> IO Text
readTextUtf8 fp = do
  bs <- readFileBS fp
  TE.decodeUtf8' bs & \case
    Left _ -> E.throwIO (FileDecodeError fp)
    Right t -> pure t
