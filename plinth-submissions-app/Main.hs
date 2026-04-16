{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Main (main) where

import Prelude

import Ecd (ecdCode)
import Factorial (factorialCode)
import Fibonacci (fibonacciCode)
import FibonacciIterative (fibonacciIterativeCode)
import LinearVesting (linearVestingValidator)
import PlutusCore.Pretty qualified as PP
import PlutusCore.Quote (runQuoteT)
import PlutusTx qualified
import PlutusTx.Code (CompiledCode, getPlcNoAnn)
import PlutusTx.Prelude (BuiltinUnit)
import PlutusTx.Builtins.Internal (BuiltinData)
import TwoPartyEscrow (twoPartyEscrowValidatorCode)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (unDeBruijnTerm)

#ifdef PREVIEW
-- Preview modules (re-compiled with BuiltinCasing)
import Preview.Ecd qualified as Preview
import Preview.Factorial qualified as Preview
import Preview.Fibonacci qualified as Preview
import Preview.FibonacciIterative qualified as Preview
import Preview.LinearVesting qualified as Preview
import Preview.TwoPartyEscrow qualified as Preview
#endif

-- Compile splice here (not in LinearVesting.hs) as workaround for
-- PlutusTx plugin bug: having compile splice without BuiltinCasing
-- in the source module prevents cross-library BuiltinCasing re-compilation.
linearVestingValidatorCode :: CompiledCode (BuiltinData -> BuiltinUnit)
linearVestingValidatorCode = $$(PlutusTx.compile [||linearVestingValidator||])

main :: IO ()
main = do
  writeCodeToFile
    "submissions/ecd/Plinth_1.45.0.0_Unisay/ecd.uplc"
    ecdCode
  writeCodeToFile
    "submissions/fibonacci_naive_recursion/Plinth_1.45.0.0_Unisay/fibonacci.uplc"
    fibonacciCode
  writeCodeToFile
    "submissions/fibonacci/Plinth_1.45.0.0_Unisay/fibonacci.uplc"
    fibonacciIterativeCode
  writeCodeToFile
    "submissions/factorial_naive_recursion/Plinth_1.45.0.0_Unisay/factorial.uplc"
    factorialCode
  writeCodeToFile
    "submissions/linear_vesting/Plinth_1.45.0.0_Unisay/linear_vesting.uplc"
    linearVestingValidatorCode
  writeCodeToFile
    "submissions/two_party_escrow/Plinth_1.45.0.0_Unisay/two_party_escrow.uplc"
    twoPartyEscrowValidatorCode
#ifdef PREVIEW
  -- Preview submissions: re-compiled with BuiltinCasing via Preview.* modules
  writeCodeToFile
    "submissions/linear_vesting/Plinth_1.61.0.0_Unisay/linear_vesting.uplc"
    Preview.linearVestingValidatorCode
  writeCodeToFile
    "submissions/ecd/Plinth_1.61.0.0_Unisay/ecd.uplc"
    Preview.ecdCode
  writeCodeToFile
    "submissions/factorial_naive_recursion/Plinth_1.61.0.0_Unisay/factorial.uplc"
    Preview.factorialCode
  writeCodeToFile
    "submissions/fibonacci_naive_recursion/Plinth_1.61.0.0_Unisay/fibonacci.uplc"
    Preview.fibonacciCode
  writeCodeToFile
    "submissions/fibonacci/Plinth_1.61.0.0_Unisay/fibonacci.uplc"
    Preview.fibonacciIterativeCode
  writeCodeToFile
    "submissions/two_party_escrow/Plinth_1.61.0.0_Unisay/two_party_escrow.uplc"
    Preview.twoPartyEscrowValidatorCode
#endif

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
    Left (err :: UPLC.FreeVariableError) -> putTextLn $ "Error converting DeBruijn names: " <> show err
    Right prettyUplc -> do
      writeFile filePath (show prettyUplc)
      putStrLn $ filePath <> " written."
