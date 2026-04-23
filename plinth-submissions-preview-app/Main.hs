{- | Generator for the preview (Plinth 1.61) submission artefacts.
Writes only the 1.61 @.uplc@ paths. Uses precompiled @Preview.*Code@
values from the @cape-preview@ sublibrary, which compiles every
validator with @BuiltinCasing@ enabled under @cabal.project.preview@.
-}
module Main (main) where

import Prelude

import Cape.WritePlc (writeCodeToFile)
import Preview.Ecd (ecdCode)
import Preview.Factorial (factorialCode)
import Preview.Fibonacci (fibonacciCode)
import Preview.FibonacciIterative (fibonacciIterativeCode)
import Preview.LinearVesting (linearVestingValidatorCode)
import Preview.TwoPartyEscrow (twoPartyEscrowValidatorCode)

main :: IO ()
main = do
  writeCodeToFile
    "submissions/ecd/Plinth_1.61.0.0_Unisay/ecd.uplc"
    ecdCode
  writeCodeToFile
    "submissions/fibonacci_naive_recursion/Plinth_1.61.0.0_Unisay/fibonacci.uplc"
    fibonacciCode
  writeCodeToFile
    "submissions/fibonacci/Plinth_1.61.0.0_Unisay/fibonacci.uplc"
    fibonacciIterativeCode
  writeCodeToFile
    "submissions/factorial_naive_recursion/Plinth_1.61.0.0_Unisay/factorial.uplc"
    factorialCode
  writeCodeToFile
    "submissions/linear_vesting/Plinth_1.61.0.0_Unisay/linear_vesting.uplc"
    linearVestingValidatorCode
  writeCodeToFile
    "submissions/two_party_escrow/Plinth_1.61.0.0_Unisay/two_party_escrow.uplc"
    twoPartyEscrowValidatorCode
