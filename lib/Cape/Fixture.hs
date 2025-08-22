module Cape.Fixture (dummyValidator) where

import PlutusCore qualified as PLC
import PlutusCore.MkPlc (mkConstant)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Core.Type (Term (..))

-- | Dummy validator UPLC program that ignores input and returns unit
dummyValidator ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan
dummyValidator =
  let ann = PLC.SrcSpan "" 1 1 1 1
      -- Create a lambda that ignores its input and returns unit constant
      -- \(x : Data) -> ()
      inputName = UPLC.Name "x" (PLC.Unique 0)
      unitConstant = mkConstant ann ()
      lambdaTerm = LamAbs ann inputName unitConstant
      version = PLC.Version 1 1 0
   in UPLC.Program ann version lambdaTerm
