module Main (main) where

import Prelude

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.SatInt (unSatInt)
import Data.Text.IO qualified as T
import PlutusCore qualified as PLC
import PlutusCore.Annotation (SrcSpans (..))
import PlutusCore.Error (ParserErrorBundle (..))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code (CompiledCodeIn (..), countAstNodes)
import PlutusTx.Eval (EvalResult (..), evaluateCompiledCode)
import System.Environment qualified as Env
import System.Exit qualified as Exit
import Text.Megaparsec (errorBundlePretty)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (deBruijnTerm)
import UntypedPlutusCore.Parser qualified as UPLC (parseProgram)

main :: IO ()
main = do
  args <- Env.getArgs
  case parseArgs args of
    Just (inputFile, outputFile) -> measureUplcFile inputFile outputFile
    Nothing -> Exit.die "Usage: measure -i <uplc-file> -o <metrics-file>"

parseArgs :: [String] -> Maybe (FilePath, FilePath)
parseArgs ["-i", inputFile, "-o", outputFile] = Just (inputFile, outputFile)
parseArgs _ = Nothing

measureUplcFile :: FilePath -> FilePath -> IO ()
measureUplcFile uplcFile metricsFile = do
  -- 1. Read pretty-printed UPLC text
  uplcText <- T.readFile uplcFile

  -- 2. Parse the UPLC text getting a value of type 'UPLC.Program'

  program <-
    case PLC.runQuoteT (UPLC.parseProgram uplcText) of
      Left (ParseErrorB err) -> error $ toText $ errorBundlePretty err
      Right prog -> pure prog

  -- 3. Convert UPLC Program to a value of type 'CompiledCode'

  -- Convert names to NamedDeBruijn indices for CompiledCode
  code <- case deBruijnTerm (UPLC._progTerm program) of
    Right termWithDeBruijn -> do
      let -- Convert annotation from SrcSpan to SrcSpans
          termWithSrcSpans = (\_ -> SrcSpans mempty) <$> termWithDeBruijn
          programWithDeBruijn =
            UPLC.Program
              (SrcSpans mempty)
              (UPLC._progVer program)
              termWithSrcSpans
      pure $ DeserializedCode programWithDeBruijn Nothing mempty
    Left err ->
      error $
        "Failed to convert names to DeBruijn indices: "
          <> show err

  -- 4. Measure compiled code.
  let EvalResult
        { evalResultBudget =
          ExBudget
            { exBudgetCPU = ExCPU cpu
            , exBudgetMemory = ExMemory mem
            }
        } = evaluateCompiledCode code
      scriptBytes = SBS.fromShort (serialiseCompiledCode code)
      scriptSize = BS.length scriptBytes

  -- 5. Write metrics to JSON file
  LBS.writeFile metricsFile $
    AesonPretty.encodePretty
      Metrics
        { cpu_units = fromIntegral (unSatInt cpu)
        , memory_units = fromIntegral (unSatInt mem)
        , script_size_bytes = fromIntegral scriptSize
        , term_size = countAstNodes code
        }
  putStrLn $ "Metrics written to " <> metricsFile
  putStrLn $ "Script size: " <> show scriptSize <> " bytes"
  putStrLn $ "CPU units: " <> show cpu
  putStrLn $ "Memory units: " <> show mem
  putStrLn $ "Term size: " <> show (countAstNodes code)

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
