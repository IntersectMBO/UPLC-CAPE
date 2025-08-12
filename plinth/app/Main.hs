module Main (main) where

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.SatInt (unSatInt)
import Fibonacci (fibonacci25Code)
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (..),
  exBudgetCPU,
  exBudgetMemory,
 )
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusCore.Pretty qualified as PP
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusTx.Code (countAstNodes, getPlc)
import PlutusTx.Test (EvalResult (..), evalResultBudget, evaluateCompiledCode)
import Prelude (IO, Integer, fromIntegral, putStrLn, show, writeFile, ($), (<>))

main :: IO ()
main = do
  -- Write pretty printed UPLC program to file
  let uplcProgram = getPlc fibonacci25Code
      prettyUplc = PP.prettyPlcClassic uplcProgram
  writeFile "fibonacci.uplc" (show prettyUplc)
  putStrLn "fibonacci.uplc written."

  -- Evaluate the compiled code and extract metrics
  let EvalResult
        { evalResultBudget =
          ExBudget
            { exBudgetCPU = ExCPU cpu
            , exBudgetMemory = ExMemory mem
            }
        } = evaluateCompiledCode fibonacci25Code
      scriptBytes = SBS.fromShort (serialiseCompiledCode fibonacci25Code)
      scriptSize = BS.length scriptBytes

  -- Write metrics to JSON file
  LBS.writeFile "metrics.json" $
    AesonPretty.encodePretty
      Metrics
        { cpu_units = fromIntegral (unSatInt cpu)
        , memory_units = fromIntegral (unSatInt mem)
        , script_size_bytes = fromIntegral scriptSize
        , term_size = countAstNodes fibonacci25Code
        }
  putStrLn "metrics.json written."
  putStrLn $ "Script size: " <> show scriptSize <> " bytes"

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
