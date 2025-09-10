module Cape.Compile (compileProgram) where

import Prelude

import PlutusCore qualified as PLC
import PlutusCore.Annotation (SrcSpans (..))
import PlutusCore.MkPlc qualified as MkPlc
import PlutusLedgerApi.Data.V3
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code (CompiledCodeIn (..))
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (deBruijnTerm)

{- | Unified program compilation with optional BuiltinData application
TODO: rename it to `programToCompiledCode` or similar
TODO: instead of passing optional builtin data, callers could use `unsafeApplyCode` with `liftCodeDef`. This makes this function simpler.
-}
compileProgram ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  Maybe BuiltinData ->
  IO (CompiledCodeIn PLC.DefaultUni PLC.DefaultFun ())
compileProgram program mBuiltinData = do
  -- Extract term and version via pattern matching for portability across Plutus versions
  let UPLC.Program _ ver term = program
  case deBruijnTerm term of
    Right termWithDeBruijn -> do
      let ann = SrcSpans mempty
          termWithSrcSpans = ann <$ termWithDeBruijn

      -- Apply BuiltinData if provided
      finalTerm <- case mBuiltinData of
        Nothing -> pure termWithSrcSpans
        Just builtinData -> do
          let coreData = Builtins.builtinDataToData builtinData
              dataConstant = MkPlc.mkConstant ann coreData
              appTerm = UPLC.Apply ann termWithSrcSpans dataConstant
          pure appTerm

      let finalProgram = UPLC.Program ann ver finalTerm
      pure $ DeserializedCode finalProgram Nothing mempty
    Left err -> die ("Failed to convert names to DeBruijn indices: " <> show err)
