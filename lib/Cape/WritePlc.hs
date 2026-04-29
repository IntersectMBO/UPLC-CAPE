{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared helper for writing CompiledCode values out as pretty-printed UPLC.
module Cape.WritePlc (writeCodeToFile) where

import Prelude

import Data.List qualified as L
import PlutusCore.Pretty qualified as PP
import PlutusCore.Quote (runQuoteT)
import PlutusTx.Code (CompiledCode, getPlcNoAnn)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (unDeBruijnTerm)

-- | Pretty-print the compiled UPLC with human-readable names and write to disk.
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath code = do
  let uplcProgramWithDeBruijn = getPlcNoAnn code
  result <- runQuoteT $ runExceptT $ do
    termWithNames <- unDeBruijnTerm (UPLC._progTerm uplcProgramWithDeBruijn)
    let programWithNames =
          UPLC.Program
            (UPLC._progAnn uplcProgramWithDeBruijn)
            (UPLC._progVer uplcProgramWithDeBruijn)
            termWithNames
    pure $ PP.prettyPlcClassic programWithNames
  case result of
    Left (err :: UPLC.FreeVariableError) ->
      putTextLn $ "Error converting DeBruijn names: " <> show err
    Right prettyUplc -> do
      -- Match the trailing-newline contract enforced by the pretty-uplc
      -- formatter so freshly generated submissions don't show a diff under
      -- treefmt.
      let normalised = L.dropWhileEnd (== '\n') (show prettyUplc) <> "\n"
      writeFile filePath normalised
      putStrLn $ filePath <> " written."
