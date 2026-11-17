module Cape.PrettyResult (
  EvalResult (..),
  TraceMode (..),
  evaluateTerm,
  evaluateCompiledCode,
  extractPrettyResult,
  getPrettyValue,
  compareResult,
) where

import Prelude

import Cape.Tests (ResultType (..))
import Data.Text (pack)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (
  defaultCekParametersForTesting,
 )
import PlutusCore.Pretty (prettyPlcClassic, render)
import PlutusTx.Code (CompiledCodeIn, getPlcNoAnn)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek (
  CekEvaluationException,
  CekReport (..),
  TallyingSt (..),
  cekResultToEither,
  logEmitter,
  noEmitter,
  runCekDeBruijn,
  tallying,
 )

-- | Evaluate UPLC term and return result as EvalResult-like structure
data EvalResult = EvalResult
  { evalResult ::
      Either
        (CekEvaluationException UPLC.NamedDeBruijn DefaultUni DefaultFun)
        (UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ())
  , evalResultBudget :: ExBudget
  , evalResultTraces :: [Text]
  {- ^ The log of the machine, in the order the program emitted it; empty
  under 'DropTraces'. See 'TraceMode' for what the log holds.
  -}
  }
  deriving stock (Show)

{- | Whether the CEK machine keeps its log. The log holds the messages of the
@trace@ builtin and the diagnostic of a builtin that fails, for example
@unIData@ on a value that is not an integer. The machine does not mark which
is which. The budget is the same in both modes.
-}
data TraceMode = DropTraces | KeepTraces
  deriving stock (Show, Eq)

-- | Evaluate a UPLC term using the CEK machine
evaluateTerm ::
  TraceMode ->
  UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun () ->
  EvalResult
evaluateTerm traceMode term =
  let emitter = case traceMode of
        DropTraces -> noEmitter
        KeepTraces -> logEmitter
      CekReport cekResult (TallyingSt _tally budget) traces =
        runCekDeBruijn defaultCekParametersForTesting tallying emitter term
      result = cekResultToEither cekResult
   in EvalResult
        { evalResult = result
        , evalResultBudget = budget
        , evalResultTraces = traces
        }

-- | Evaluate compiled code using the CEK machine
evaluateCompiledCode ::
  TraceMode -> CompiledCodeIn DefaultUni DefaultFun a -> EvalResult
evaluateCompiledCode traceMode code =
  let UPLC.Program _ _ term = getPlcNoAnn code
   in evaluateTerm traceMode term

-- | Extract pretty-printed result from evaluation
extractPrettyResult :: EvalResult -> Either Text Text
extractPrettyResult EvalResult {evalResult = result} =
  bimap (pack . show) (render . prettyPlcClassic) result

-- | Get success value as pretty-printed text (fails if result is error)
getPrettyValue :: EvalResult -> Maybe Text
getPrettyValue EvalResult {evalResult = result} =
  case result of
    Left _ -> Nothing
    Right term -> Just (render (prettyPlcClassic term))

-- | Compare evaluation result with expected outcome
compareResult :: EvalResult -> ResultType -> Maybe Text -> Bool
compareResult evalRes expectedType expectedContent =
  case expectedType of
    ExpectedError -> isLeft (evalResult evalRes)
    ExpectedValue ->
      case (getPrettyValue evalRes, expectedContent) of
        (Just actualValue, Just expected) -> actualValue == expected
        (Just _, Nothing) -> False
        (Nothing, _) -> False
