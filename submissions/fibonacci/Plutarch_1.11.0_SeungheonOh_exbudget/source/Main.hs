{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Compile (compileToUplc)
import Plutarch.Internal.Term (punsafeBuiltin, punsafeCoerce)
import Plutarch.Prelude
import PlutusCore qualified as PLC

-- conditional without hoisting the IfThenElse builtin.
pif'' :: Term s PBool -> Term s a -> Term s a -> Term s a
pif'' cond ifT ifF =
  pforce $
    pforce (punsafeBuiltin PLC.IfThenElse) # cond # pdelay ifT # pdelay ifF

-- pfix but inlines the recursion point twice.
pfix' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix' f = plam (\r -> f (punsafeCoerce r # r)) # plam (\r -> f (punsafeCoerce r # r))

pfibo :: Term s (PInteger :--> PInteger)
pfibo =
  pfix' $ \r -> plam $ \x ->
    pif'' (x #<= 1) x (r # (x - 1) + r # (x - 2))

main :: IO ()
main = compileToUplc "fibonacci_exbudget.uplc" pfibo
