module Main (main) where

import Compile (compileToUplc)
import Plutarch.Prelude

pfiboNaive :: Term s (PInteger :--> PInteger)
pfiboNaive =
  pfix #$ plam $ \self n ->
    pif (n #<= 1) n (self # (n - 1) + self # (n - 2))

main :: IO ()
main = compileToUplc "fibonacci_naive_recursion.uplc" pfiboNaive
