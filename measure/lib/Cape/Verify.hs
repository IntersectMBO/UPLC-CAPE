module Cape.Verify (isBuiltinUnit) where

import Prelude

import Data.Some qualified as Some
import PlutusCore qualified as PLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (NamedDeBruijn)

-- | Heuristically detect the unit constant in an evaluated term.
-- This matches the canonical 'con unit ()' form.
isBuiltinUnit ::
  UPLC.Term NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () -> Bool
isBuiltinUnit = \case
  UPLC.Constant _ (Some.Some (PLC.ValueOf PLC.DefaultUniUnit ())) -> True
  _ -> False
