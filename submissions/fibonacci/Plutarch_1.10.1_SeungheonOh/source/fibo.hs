module Main where

import Control.Lens (traverseOf)
import Plutarch.Prelude
import Plutarch.Internal.Term (compile, punsafeCoerce)
import Plutarch.Script
import PlutusCore.Pretty
import UntypedPlutusCore (programMapNames, fakeNameDeBruijn, progTerm, unDeBruijnTerm)
import PlutusCore (runQuoteT, FreeVariableError)

-- this is faster than `Plutarch.Internal.Fix.pfix`.
pfix' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix' f =
  (plam $ \r -> (punsafeCoerce r) # r) #
    (plam $ \r -> f ((punsafeCoerce r) # r))

-- this is faster than `pfix'` but generates more bloat uplc.
pfix'' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix'' f =
  (plam $ \r -> f ((punsafeCoerce r) # r)) #
    (plam $ \r -> f ((punsafeCoerce r) # r))

pfibo :: Term s (PInteger :--> PInteger)
pfibo =
  pfix' $ \r -> plam $ \x ->
    pif (x #<= 1) x (r # (x - 1) + r # (x - 2))

main :: IO ()
main =
  case compile mempty $ pfibo # 25 of
    Left _ -> error "compiliation failed"
    Right (Script s) ->
      case runQuoteT $ traverseOf progTerm unDeBruijnTerm $ programMapNames fakeNameDeBruijn s of
        Left (_ :: FreeVariableError) -> error "debruijn conversion failed"
        Right s' -> print $ prettyPlcClassic s'
