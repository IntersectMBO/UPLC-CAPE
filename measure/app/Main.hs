module Main (main) where

import Prelude

import Control.Exception qualified as E
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.SatInt (unSatInt)
import Options.Applicative
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code (countAstNodes)
import PlutusTx.Eval (EvalResult (..), displayEvalResult, evaluateCompiledCode)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.Exit qualified as Exit
import System.Process (readProcess)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLCParser

import App.Compile (applyPrograms, compileProgram)
import App.Verify (isBuiltinUnit)
import Data.Text.Encoding qualified as TE

-- CLI options
-- -i: input UPLC file
-- -o: output metrics.json file
-- -v: optional verifier UPLC file (applied as (verifier submissionResult))
data Options = Options
  { optInput :: FilePath
  , optOutput :: FilePath
  , optVerifier :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      (long "input" <> short 'i' <> metavar "FILE" <> help "UPLC input file")
    <*> strOption
      ( long "output" <> short 'o' <> metavar "FILE" <> help "metrics.json output file"
      )
    <*> optional
      ( strOption
          ( long "verifier"
              <> short 'v'
              <> metavar "VERIFIER.uplc"
              <> help
                "Verifier UPLC file applied as (verifier submissionResult) for correctness verification"
          )
      )

optsInfo :: ParserInfo Options
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Measure a UPLC program and optionally verify correctness using a provided verifier UPLC file"
        <> header "uplc-measure"
    )

main :: IO ()
main = do
  Options {optInput, optOutput, optVerifier} <- execParser optsInfo
  measureUplcFile optVerifier optInput optOutput

-- Helper: strict UTF-8 file reader with graceful failure
readTextUtf8 :: FilePath -> IO Text
readTextUtf8 fp = do
  bs <- readFileBS fp
  case TE.decodeUtf8' bs of
    Left _ -> do
      putTextLn $ "Failed to decode UTF-8 text: " <> toText fp
      Exit.exitWith (ExitFailure 4)
    Right t -> pure t

measureUplcFile :: Maybe FilePath -> FilePath -> FilePath -> IO ()
measureUplcFile mVerifier uplcFile metricsFile = do
  -- 1. Read pretty-printed UPLC text
  uplcText <- readTextUtf8 uplcFile

  -- 2. Parse the UPLC text getting a value of type 'UPLC.Program'
  program <- do
    let parsed = runExceptT (UPLCParser.parseProgram uplcText)
    case PLC.runQuote parsed of
      Left err -> do
        putTextLn $ "Malformed/invalid UPLC file: " <> toText uplcFile
        putTextLn $ toText @String (show err)
        exitWith (ExitFailure 4)
      Right prog -> pure prog

  -- Convert UPLC Program to a value of type 'CompiledCode'
  code <- compileProgram program

  -- Evaluate submission with CEK machine (same settings used for measurement)
  let evalRes = evaluateCompiledCode code
  case evalResult evalRes of
    Left _ -> do
      putTextLn $
        "Verification/evaluation failed while evaluating submission: "
          <> displayEvalResult evalRes
      Exit.exitWith (ExitFailure 3)
    Right valueTerm -> do
      if isBuiltinUnit valueTerm
        then putTextLn "Submission result is BuiltinUnit; verifier not required."
        else case mVerifier of
          Nothing -> do
            putTextLn
              "Verifier required: submission result is not BuiltinUnit. Provide -v/--verifier <verifier.uplc>."
            Exit.exitWith (ExitFailure 2)
          Just verifierPath -> do
            exists <- doesFileExist verifierPath
            if not exists
              then do
                putTextLn $ "Verifier file not found: " <> toText verifierPath
                Exit.exitWith (ExitFailure 2)
              else do
                verifierText <- readTextUtf8 verifierPath
                let parsedVerifier = runExceptT (UPLCParser.parseProgram verifierText)
                verifierProg <-
                  case PLC.runQuote parsedVerifier of
                    Left err -> do
                      putTextLn $ "Malformed/invalid verifier UPLC file: " <> toText verifierPath
                      putTextLn $ toText @String (show err)
                      Exit.exitWith (ExitFailure 4)
                    Right p -> pure p
                -- Build application: (verifierProg submissionProgram)
                appCode <- applyPrograms verifierProg program
                let testRes = evaluateCompiledCode appCode
                case evalResult testRes of
                  Right term' | isBuiltinUnit term' -> do
                    putTextLn "Verification passed."
                    putTextLn ("Verifier evaluation summary: " <> displayEvalResult testRes)
                  _ -> do
                    putTextLn
                      "Verification failed: (verifier submissionResult) did not reduce to BuiltinUnit."
                    putTextLn ("Verifier evaluation summary: " <> displayEvalResult testRes)
                    Exit.exitWith (ExitFailure 3)

  -- At this point verification has passed or was not required; proceed to metrics
  let EvalResult
        { evalResultBudget =
          ExBudget {exBudgetCPU = ExCPU cpu, exBudgetMemory = ExMemory mem}
        } = evalRes
      scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes
      -- Extract version via pattern matching to avoid using internal accessors
      verStr :: String
      verStr = case program of
        UPLC.Program _ (PLC.Version major minor patch) _ -> show major ++ "." ++ show minor ++ "." ++ show patch

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
  putTextLn "Evaluation summary:"
  putTextLn (displayEvalResult evalRes)
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
  pure $ case eres of
    Right out ->
      let verTok = takeWhile (\c -> c /= ' ' && c /= '\n') out
       in if null verTok then "unknown" else toText verTok
    Left (_ :: E.SomeException) -> "unknown"
