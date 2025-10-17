module Main (main) where

import Compile (compileToUplc)
import Plutarch.Prelude

pfactorialNaive :: Term s (PInteger :--> PInteger)
pfactorialNaive =
  pfix #$ plam $ \self n ->
    pif (n #<= 0) 1 (n * (self # (n - 1)))

main :: IO ()
main = compileToUplc "factorial_naive_recursion.uplc" pfactorialNaive
