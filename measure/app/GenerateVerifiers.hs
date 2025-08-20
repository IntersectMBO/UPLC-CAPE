{-# LANGUAGE Strict #-}
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

module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import PlutusCore.Pretty qualified as PP
import PlutusCore.Quote (runQuoteT)
import PlutusTx qualified as Tx
import PlutusTx.Builtins.Internal (unitval)
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

-- | Dummy validator that always returns unit regardless of input
dummyValidator :: BuiltinData -> BuiltinUnit
dummyValidator _ = unitval

-- | Compile the dummy validator to UPLC
dummyValidatorCompiled :: Tx.CompiledCode (BuiltinData -> BuiltinUnit)
dummyValidatorCompiled = $$(Tx.compile [||dummyValidator||])

-- | Test validator for integration testing (accepts integer 42 as BuiltinData)
testValidator :: BuiltinData -> BuiltinUnit
testValidator input =
  case Tx.fromBuiltinData input of
    Just (42 :: Integer) -> unitval
    _ -> traceError "Invalid input: expected 42"

-- | Compile the test validator to UPLC
testValidatorCompiled :: Tx.CompiledCode (BuiltinData -> BuiltinUnit)
testValidatorCompiled = $$(Tx.compile [||testValidator||])

-- | Generate UPLC text for the specified subject
generateUPLC :: Text -> IO Text
generateUPLC subject = do
  uplcProgramWithDeBruijn <- case subject of
    "equals42" -> return $ Tx.getPlcNoAnn verifyEquals42Compiled
    "dummy-validator" -> return $ Tx.getPlcNoAnn dummyValidatorCompiled
    "test-validator" -> return $ Tx.getPlcNoAnn testValidatorCompiled
    _ -> do
      putStrLn $ "Error: Unknown subject '" ++ Text.unpack subject ++ "'"
      putStrLn "Available subjects: equals42, dummy-validator, test-validator"
      exitFailure

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

-- | Write the UPLC program to a file
writeUPLCToFile :: Text -> FilePath -> IO ()
writeUPLCToFile subject filepath = do
  uplcText <- generateUPLC subject
  Text.writeFile filepath uplcText
  putStrLn $ "UPLC program ("
    ++ Text.unpack subject
    ++ ") written to: "
    ++ filepath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [subject, filepath] -> writeUPLCToFile (Text.pack subject) filepath
    [subject] -> do
      uplcText <- generateUPLC (Text.pack subject)
      Text.putStrLn uplcText
    _ -> do
      putStrLn "Usage: generate-verifiers <subject> [output-file]"
      putStrLn "  subject: equals42, dummy-validator, test-validator"
      putStrLn "  Without output-file: prints UPLC to stdout"
      putStrLn "  Subject parameter is required"
      exitFailure
