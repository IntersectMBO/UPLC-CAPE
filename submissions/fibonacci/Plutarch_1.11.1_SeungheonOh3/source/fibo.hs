module Main where

import Control.Lens (traverseOf)
import Plutarch.Internal.Term (compile, punsafeCoerce)
import Plutarch.Prelude
import Plutarch.Script
import PlutusCore (FreeVariableError, runQuoteT)
import PlutusCore.Pretty
import UntypedPlutusCore (
  fakeNameDeBruijn,
  progTerm,
  programMapNames,
  unDeBruijnTerm,
 )

-- this is faster than `Plutarch.Internal.Fix.pfix`.
pfix' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix' f =
  (plam $ \r -> (punsafeCoerce r) # r)
    # (plam $ \r -> f ((punsafeCoerce r) # r))

-- this is faster than `pfix'` but generates more bloat uplc.
pfix'' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix'' f =
  (plam $ \r -> f ((punsafeCoerce r) # r))
    # (plam $ \r -> f ((punsafeCoerce r) # r))

pfibo :: Term s (PInteger :--> PInteger)
pfibo = plam $ \x ->
  pif
    (x #< 0)
    x
    ( (pfix' $ \r -> plam $ \a b x -> pif (x #== 0) a (r # b # (a + b) # (x - 1)))
        # 0
        # 1
        # x
    )

main :: IO ()
main =
  case compile mempty $ pfibo of
    Left _ -> error "compiliation failed"
    Right (Script s) ->
      case runQuoteT $
        traverseOf progTerm unDeBruijnTerm $
          programMapNames fakeNameDeBruijn s of
        Left (_ :: FreeVariableError) -> error "debruijn conversion failed"
        Right s' -> print $ prettyPlcClassic s'
