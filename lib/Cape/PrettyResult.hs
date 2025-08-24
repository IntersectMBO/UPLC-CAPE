module Cape.PrettyResult (
  extractPrettyResult,
  getPrettyValue,
  compareResult,
) where

import Prelude

import Cape.Tests (ResultType (..))
import PlutusCore.Pretty (prettyPlcClassic, render)
import PlutusTx.Eval (EvalResult (..), displayEvalResult, evalResult)

-- | Extract pretty-printed result from EvalResult using displayEvalResult
extractPrettyResult :: EvalResult -> Either Text Text
extractPrettyResult evalRes =
  bimap displayText displayText (evalResult evalRes)
  where
    displayText :: forall a. a -> Text
    displayText _ = displayEvalResult evalRes

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
