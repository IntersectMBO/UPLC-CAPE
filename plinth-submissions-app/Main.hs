{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{- | Generator for the production (Plinth 1.45) submission artefacts.
Writes only the 1.45 @.uplc@ paths. The preview (Plinth 1.61) generator
lives in @plinth-submissions-preview-app/Main.hs@ and uses a separate
cabal project, so neither build can accidentally overwrite the other.
-}
module Main (main) where

import Prelude

import Cape.WritePlc (writeCodeToFile)
import Ecd (ecdCode)
import Factorial (factorialCode)
import Fibonacci (fibonacciCode)
import FibonacciIterative (fibonacciIterativeCode)
import LinearVesting (linearVestingValidator)
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinData)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (BuiltinUnit)
import TwoPartyEscrow (twoPartyEscrowValidatorCode)

-- Compile splice lives here (not in LinearVesting.hs) as a workaround for a
-- PlutusTx plugin interaction: having @$$(compile ...)@ in the source module
-- without BuiltinCasing blocks cross-library recompilation with
-- BuiltinCasing in the corresponding Preview.* module.
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
