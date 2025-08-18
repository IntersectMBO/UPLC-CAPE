module App.Compile (compileProgram, applyPrograms) where

import Prelude

import PlutusCore qualified as PLC
import PlutusCore.Annotation (SrcSpans (..))
import PlutusTx.Code (CompiledCodeIn (..))
import qualified System.Exit as Exit
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.DeBruijn (deBruijnTerm)

compileProgram ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  IO (CompiledCodeIn PLC.DefaultUni PLC.DefaultFun ())
compileProgram program = do
  -- Extract term and version via pattern matching for portability across Plutus versions
  let UPLC.Program _ ver term = program
  case deBruijnTerm term of
    Right termWithDeBruijn -> do
      let ann = SrcSpans mempty
          termWithSrcSpans = (\_ -> ann) <$> termWithDeBruijn
          programWithDeBruijn = UPLC.Program ann ver termWithSrcSpans
      pure $ DeserializedCode programWithDeBruijn Nothing mempty
    Left err -> do
      putTextLn $ "Failed to convert names to DeBruijn indices: " <> show err
      Exit.exitFailure

applyPrograms ::
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  UPLC.Program UPLC.Name PLC.DefaultUni PLC.DefaultFun PLC.SrcSpan ->
  IO (CompiledCodeIn PLC.DefaultUni PLC.DefaultFun ())
applyPrograms fProg xProg = do
  let UPLC.Program _ verF fTermNamed = fProg
      UPLC.Program _ _    xTermNamed = xProg
  case (deBruijnTerm fTermNamed, deBruijnTerm xTermNamed) of
    (Right fTerm, Right xTerm) -> do
      let ann = SrcSpans mempty
          f' = (\_ -> ann) <$> fTerm
          x' = (\_ -> ann) <$> xTerm
          appTerm = UPLC.Apply ann f' x'
          appProgram = UPLC.Program ann verF appTerm
      pure $ DeserializedCode appProgram Nothing mempty
    (Left e, _) -> do
      putTextLn $ "Failed to convert verifier to DeBruijn indices: " <> show e
      Exit.exitFailure
    (_, Left e) -> do
      putTextLn $ "Failed to convert submission to DeBruijn indices: " <> show e
      Exit.exitFailure
