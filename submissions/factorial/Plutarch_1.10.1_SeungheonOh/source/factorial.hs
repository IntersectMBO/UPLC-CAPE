module Main where

import Control.Lens (traverseOf)
import Plutarch.Prelude
import Plutarch.Internal.Term (compile, punsafeCoerce, punsafeBuiltin)
import Plutarch.Script
import PlutusCore.Pretty
import UntypedPlutusCore (programMapNames, fakeNameDeBruijn, progTerm, unDeBruijnTerm)
import PlutusCore (runQuoteT, FreeVariableError)
import PlutusCore qualified as PLC

-- conditional with no hoisting
pif'' :: Term s PBool -> Term s a -> Term s a -> Term s a
pif'' cond ifT ifF =
  pforce $ (pforce $ punsafeBuiltin PLC.IfThenElse) # cond # pdelay ifT # pdelay ifF

pfix' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix' f =
  (plam $ \r -> (punsafeCoerce r) # r) #
    (plam $ \r -> f ((punsafeCoerce r) # r))

pfactorial :: Term s (PInteger :--> PInteger)
pfactorial =
  -- This is concise, but slower
  pfix' $ \r -> plam $ \x -> pif'' (x #== 0) 1 (x * (r # (x - 1)))
  -- This bloats the script size, but faster
  -- punrollBound 11 perror $ \r -> plam $ \x -> pif (x #== 1) x (x * (r # (x - 1)))

main :: IO ()
main =
  case compile mempty $ pfactorial # 10 of
    Left _ -> error "compiliation failed"
    Right (Script s) ->
      case runQuoteT $ traverseOf progTerm unDeBruijnTerm $ programMapNames fakeNameDeBruijn s of
        Left (_ :: FreeVariableError) -> error "debruijn conversion failed"
        Right s' -> print $ prettyPlcClassic s'
