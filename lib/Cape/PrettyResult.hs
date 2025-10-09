module Cape.PrettyResult (
  EvalResult (..),
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
  TallyingSt (..),
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
  }
  deriving stock (Show)

-- | Evaluate a UPLC term using the CEK machine
evaluateTerm ::
  UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun () -> EvalResult
evaluateTerm term =
  let (result, TallyingSt _tally budget, _logs) = runCekDeBruijn defaultCekParametersForTesting tallying noEmitter term
   in EvalResult
        { evalResult = result
        , evalResultBudget = budget
        }

-- | Evaluate compiled code using the CEK machine
evaluateCompiledCode :: CompiledCodeIn DefaultUni DefaultFun a -> EvalResult
evaluateCompiledCode code =
  let UPLC.Program _ _ term = getPlcNoAnn code
   in evaluateTerm term

-- | Extract pretty-printed result from evaluation
extractPrettyResult :: EvalResult -> Either Text Text
extractPrettyResult (EvalResult result _) =
  bimap (pack . show) (render . prettyPlcClassic) result

-- | Get success value as pretty-printed text (fails if result is error)
getPrettyValue :: EvalResult -> Maybe Text
getPrettyValue (EvalResult result _) =
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
