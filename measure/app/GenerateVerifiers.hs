{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
--
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import PlutusCore.Pretty qualified as PP
import PlutusCore.Quote (runQuoteT)
import PlutusTx qualified as Tx
import PlutusTx.Prelude
import System.Environment (getArgs)
import System.Exit (exitFailure)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (unDeBruijnTerm)
import Prelude (FilePath, IO, putStrLn, show, (++))
import Prelude qualified as P

-- | Verifier that checks if the input integer equals 42
verifyEquals42 :: Integer -> BuiltinUnit
verifyEquals42 n = check (n == 42)

-- | Compile the verifier to UPLC
verifyEquals42Compiled :: Tx.CompiledCode (Integer -> BuiltinUnit)
verifyEquals42Compiled = $$(Tx.compile [||verifyEquals42||])

-- | Generate UPLC text for the verifier
generateVerifierUPLC :: IO Text
generateVerifierUPLC = do
  let compiled = verifyEquals42Compiled
      uplcProgramWithDeBruijn = Tx.getPlcNoAnn compiled

  -- Convert DeBruijn names to regular names to be able to parse it
  -- using stock parser from the UntypedPlutusCore library.
  result <- runQuoteT P.$ P.runExceptT P.$ do
    termWithNames <- unDeBruijnTerm (UPLC._progTerm uplcProgramWithDeBruijn)
    let programWithNames =
          UPLC.Program
            (UPLC._progAnn uplcProgramWithDeBruijn)
            (UPLC._progVer uplcProgramWithDeBruijn)
            termWithNames
    P.return P.$ PP.prettyPlcClassic programWithNames

  case result of
    P.Left err -> do
      putStrLn P.$ "Error converting DeBruijn names: " ++ show err
      P.return "Error generating UPLC"
    P.Right prettyUplc ->
      P.return P.$ Text.pack P.$ show prettyUplc

-- | Write the verifier UPLC to a file
writeVerifierToFile :: FilePath -> IO ()
writeVerifierToFile filepath = do
  uplcText <- generateVerifierUPLC
  Text.writeFile filepath uplcText
  putStrLn P.$ "Verifier UPLC written to: " ++ filepath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> writeVerifierToFile filepath
    [] -> do
      uplcText <- generateVerifierUPLC
      Text.putStrLn uplcText
    _ -> do
      putStrLn "Usage: generate-verifiers [output-file]"
      putStrLn "  Without output-file: prints UPLC to stdout"
      putStrLn "  With output-file: writes UPLC to file"
      exitFailure
