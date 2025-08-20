{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Prelude

import App.Cli (Options (..), parseOptions)
import App.Compile (applyPrograms, compileProgram)
import App.Error (MeasureError (..), exitCodeForError, renderMeasureError)
import App.Fixture (dummyValidator)
import App.Verify (isBuiltinUnit)
import Control.Exception qualified as E
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
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code (countAstNodes)
import PlutusTx.Eval (EvalResult (..), displayEvalResult, evaluateCompiledCode)
import System.Directory (doesFileExist)
import System.Exit qualified as Exit
import System.Process (readProcess)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLCParser

main :: IO ()
main = do
  Options {optInput, optOutput, optVerifier, optDriver} <- parseOptions
  E.try (measureUplcFile optVerifier optDriver optInput optOutput) >>= \case
    Left err -> do
      putStrLn (renderMeasureError err)
      Exit.exitWith (exitCodeForError err)
    Right () -> pass

-- Helper: strict UTF-8 file reader with exception handling
readTextUtf8 :: FilePath -> IO Text
readTextUtf8 fp = do
  bs <- readFileBS fp
  TE.decodeUtf8' bs & \case
    Left _ -> E.throwIO (FileDecodeError fp)
    Right t -> pure t

-- | Calculate driver overhead by measuring driver(dummy)
calculateDriverOverhead ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  IO ExBudget
calculateDriverOverhead driverProg dummyProg = do
  putTextLn "Calculating driver overhead with dummy validator..."
  -- Apply driver to dummy: driver(dummy)
  overheadCode <- applyPrograms driverProg dummyProg
  let overheadRes = evaluateCompiledCode overheadCode
  evalResult overheadRes & \case
    Left _ ->
      E.throwIO
        ( DriverError
            ( "Driver overhead measurement failed: "
                ++ show (displayEvalResult overheadRes)
            )
        )
    Right _ -> do
      putTextLn $ "Driver overhead measured: " <> displayEvalResult overheadRes
      pure (evalResultBudget overheadRes)

-- | Measure with driver by calculating total - overhead
measureWithDriver ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  FilePath ->
  IO ExBudget
measureWithDriver driverProg validatorProg _ = do
  putTextLn "Measuring with driver architecture..."

  -- 1. Calculate overhead using dummy validator
  overhead <- calculateDriverOverhead driverProg dummyValidator

  -- 2. Measure driver(validator)
  putTextLn "Measuring driver applied to validator..."
  totalCode <- applyPrograms driverProg validatorProg
  let totalRes = evaluateCompiledCode totalCode
  evalResult totalRes & \case
    Left _ ->
      E.throwIO
        ( DriverError
            ( "Driver total measurement failed: "
                ++ show (displayEvalResult totalRes)
            )
        )
    Right _ -> do
      let totalBudget = evalResultBudget totalRes
      putTextLn $ "Driver total measured: " <> displayEvalResult totalRes

      -- 3. Calculate validator cost = total - overhead
      let ExBudget
            { exBudgetCPU = ExCPU overheadCpu
            , exBudgetMemory = ExMemory overheadMem
            } = overhead
      let ExBudget
            { exBudgetCPU = ExCPU totalCpu
            , exBudgetMemory = ExMemory totalMem
            } = totalBudget
      let validatorCpu = totalCpu - overheadCpu
      let validatorMem = totalMem - overheadMem
      let validatorBudget = ExBudget (ExCPU validatorCpu) (ExMemory validatorMem)

      putTextLn $ "Calculated validator cost (total - overhead):"
      putTextLn $ "  CPU units: " <> toText @String (show validatorCpu)
      putTextLn $ "  Memory units: " <> toText @String (show validatorMem)

      pure validatorBudget

measureUplcFile ::
  Maybe FilePath -> Maybe FilePath -> FilePath -> FilePath -> IO ()
measureUplcFile mVerifier mDriver uplcFile metricsFile = do
  -- 1. Read pretty-printed UPLC text
  uplcText <- readTextUtf8 uplcFile

  -- 2. Parse the UPLC text getting a value of type 'UPLC.Program'
  program <-
    case PLC.runQuote (runExceptT (UPLCParser.parseProgram uplcText)) of
      Left err -> E.throwIO (UPLCParseError uplcFile (show err))
      Right prog -> pure prog

  -- Convert UPLC Program to a value of type 'CompiledCode'
  code <- compileProgram program

  -- Verification: skip direct evaluation when using driver (driver will provide necessary input)
  case mDriver of
    Just _ -> do
      putTextLn
        "Driver mode: skipping direct evaluation, will use driver for verification"
    Nothing -> do
      -- Evaluate submission with CEK machine (same settings used for measurement)
      let evalRes = evaluateCompiledCode code
      evalResult evalRes & \case
        Left _ ->
          E.throwIO
            ( EvaluationError
                ( "Verification/evaluation failed while evaluating submission: "
                    ++ show (displayEvalResult evalRes)
                )
            )
        Right valueTerm -> do
          if isBuiltinUnit valueTerm
            then putTextLn "Submission result is BuiltinUnit; verifier not required."
            else
              mVerifier & \case
                Nothing ->
                  E.throwIO
                    ( VerificationError
                        "Verifier required: submission result is not BuiltinUnit. Provide -v/--verifier <verifier.uplc>."
                    )
                Just verifierPath -> do
                  exists <- doesFileExist verifierPath
                  if not exists
                    then E.throwIO (FileNotFoundError verifierPath)
                    else do
                      verifierText <- readTextUtf8 verifierPath
                      let parsedVerifier = runExceptT (UPLCParser.parseProgram verifierText)
                      verifierProg <-
                        PLC.runQuote parsedVerifier & \case
                          Left err -> E.throwIO (UPLCParseError verifierPath (show err))
                          Right p -> pure p
                      -- Build application: (verifierProg submissionProgram)
                      appCode <- applyPrograms verifierProg program
                      let testRes = evaluateCompiledCode appCode
                      case evalResult testRes of
                        Right term' | isBuiltinUnit term' -> do
                          putTextLn "Verification passed."
                          putTextLn
                            ( "Verifier evaluation summary: "
                                <> displayEvalResult testRes
                            )
                        _ ->
                          E.throwIO
                            ( VerificationError
                                ( "Verification failed: (verifier submissionResult) did not reduce to BuiltinUnit.\nVerifier evaluation summary: "
                                    ++ show (displayEvalResult testRes)
                                )
                            )

  -- At this point verification has passed or was not required; proceed to metrics
  finalBudget <- case mDriver of
    Nothing -> do
      -- Standard measurement: use direct evaluation results
      putTextLn "Using standard measurement (no driver)"
      -- Need to re-evaluate since evalRes might not be in scope in driver mode
      let evalRes = evaluateCompiledCode code
      pure (evalResultBudget evalRes)
    Just driverPath -> do
      -- Driver measurement: calculate overhead and subtract
      putTextLn $ "Using driver-based measurement: " <> toText driverPath
      exists <- doesFileExist driverPath
      if not exists
        then E.throwIO (FileNotFoundError driverPath)
        else do
          driverText <- readTextUtf8 driverPath
          let parsedDriver = runExceptT (UPLCParser.parseProgram driverText)
          driverProg <-
            PLC.runQuote parsedDriver & \case
              Left err -> E.throwIO (UPLCParseError driverPath (show err))
              Right p -> pure p
          measureWithDriver driverProg program metricsFile

  let ExBudget
        { exBudgetCPU = ExCPU cpu
        , exBudgetMemory = ExMemory mem
        } = finalBudget
      scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes
      -- Extract version via pattern matching to avoid using internal accessors
      verStr :: String
      verStr = case program of
        UPLC.Program _ (PLC.Version major minor patch) _ ->
          show major ++ "." ++ show minor ++ "." ++ show patch

      plutusCoreVersion :: Text
      plutusCoreVersion = toText @String verStr

  evaluatorVersion <- getEvaluatorVersion
  let evaluatorString :: Text
      evaluatorString = "PlutusTx.Eval-" <> evaluatorVersion

  -- Write metrics to JSON file
  LBS.writeFile metricsFile $
    AesonPretty.encodePretty
      Metrics
        { cpu_units = fromIntegral (unSatInt cpu)
        , memory_units = fromIntegral (unSatInt mem)
        , script_size_bytes = fromIntegral scriptSize
        , term_size = countAstNodes code
        }
  putTextLn $ "Metrics written to " <> toText metricsFile
  let evalMode = case mDriver of
        Nothing -> "Standard evaluation"
        Just _ -> "Driver-based evaluation (validator cost after overhead subtraction)"
  putTextLn $ "Final metrics (" <> evalMode <> "):"
  putTextLn $ "Script size: " <> toText @String (show scriptSize) <> " bytes"
  putTextLn $ "CPU units: " <> toText @String (show cpu)
  putTextLn $ "Memory units: " <> toText @String (show mem)
  putTextLn $ "Term size: " <> toText @String (show (countAstNodes code))
  putTextLn $ "Plutus Core version: " <> plutusCoreVersion
  putTextLn $ "Evaluator: " <> evaluatorString

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

getEvaluatorVersion :: IO Text
getEvaluatorVersion = do
  eres <- E.try (readProcess "uplc" ["--version"] "")
  pure $
    eres & \case
      Right out ->
        let verTok = takeWhile (\c -> c /= ' ' && c /= '\n') out
         in if null verTok then "unknown" else toText verTok
      Left (_ :: E.SomeException) -> "unknown"
