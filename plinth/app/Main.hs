module Main (main) where

import Prelude

import Fibonacci (fibonacci25Code)
import PlutusCore.Pretty qualified as PP
import PlutusCore.Quote (runQuoteT)
import PlutusTx.Code (CompiledCode, getPlcNoAnn)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (unDeBruijnTerm)

main :: IO ()
main = do
  writeCodeToFile "fibonacci.uplc" fibonacci25Code

writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath code = do
  -- Get UPLC program with DeBruijn names
  let uplcProgramWithDeBruijn = getPlcNoAnn code

  -- Convert DeBruijn names to regular names to be able to parse it
  -- using stock parser from the UntypedPlutusCore library.
  result <- runQuoteT $ runExceptT $ do
    termWithNames <- unDeBruijnTerm (UPLC._progTerm uplcProgramWithDeBruijn)
    let programWithNames =
          UPLC.Program
            (UPLC._progAnn uplcProgramWithDeBruijn)
            (UPLC._progVer uplcProgramWithDeBruijn)
            termWithNames
    pure $ PP.prettyPlcClassic programWithNames

  case result of
    Left err -> putTextLn $ "Error converting DeBruijn names: " <> show err
    Right prettyUplc -> do
      writeFile filePath (show prettyUplc)
      putStrLn $ filePath ++ " written."
