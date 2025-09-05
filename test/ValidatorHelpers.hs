{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Generic validator testing helpers

This module provides reusable functions for testing any UPLC validator,
including ScriptContext building, evaluation, and assertion helpers.
These functions can be used across different validator test suites.
-}
module ValidatorHelpers (
  -- * ScriptContext Building
  buildScriptContextOrCrash,
  buildContextData,

  -- * Validator Evaluation
  evaluateValidatorCode,
  expectSuccess,
  expectFailure,

  -- * Evaluation Result Predicates
  isEvaluationSuccess,
  isEvaluationFailure,

  -- * Value Helpers
  lovelaceValue,
  adaValue,
) where

import Prelude

import Cape.ScriptContextBuilder
import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V1.Data.Value (Lovelace (..))
import PlutusTx qualified
import PlutusTx.Eval (EvalResult (..), evaluateCompiledCode)
import PlutusTx.Prelude (BuiltinUnit)
import Test.Hspec

--------------------------------------------------------------------------------
-- ScriptContext Building Helpers ----------------------------------------------

{- | Build ScriptContext or crash with error message

This eliminates the need for case analysis in tests by converting
build failures to runtime errors with descriptive messages.
-}
buildScriptContextOrCrash ::
  HasCallStack => ScriptContextBuilder -> ScriptContext
buildScriptContextOrCrash builder =
  case buildScriptContext builder of
    Right ctx -> ctx
    Left err -> error $ "ScriptContext builder failed in test: " <> show err

{- | Build ScriptContext and convert to BuiltinData in one step

Convenience function that combines context building and conversion
to the format expected by validator functions.
-}
buildContextData :: ScriptContextBuilder -> BuiltinData
buildContextData = toBuiltinData . buildScriptContextOrCrash

--------------------------------------------------------------------------------
-- Validator Evaluation Helpers ------------------------------------------------

{- | Evaluate a compiled validator with input data

Generic function that can evaluate any validator compiled code with
the provided input data. Returns IO EvalResult for use in tests.
-}
evaluateValidatorCode ::
  HasCallStack =>
  PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit) ->
  BuiltinData ->
  IO EvalResult
evaluateValidatorCode validatorCode inputData = do
  case validatorCode `PlutusTx.applyCode` PlutusTx.liftCodeDef inputData of
    Left err -> error $ "Failed to apply code: " <> toText err
    Right appliedCode -> pure $ evaluateCompiledCode appliedCode

{- | Helper function to test that validation succeeds

Convenience function for asserting successful validator evaluation.
This creates a specialized version for a specific validator.
-}
expectSuccess ::
  HasCallStack =>
  (BuiltinData -> IO EvalResult) ->
  BuiltinData ->
  IO ()
expectSuccess evaluateFunc contextData = do
  result <- evaluateFunc contextData
  result `shouldSatisfy` isEvaluationSuccess

{- | Helper function to test that validation fails

Convenience function for asserting failed validator evaluation.
This creates a specialized version for a specific validator.
-}
expectFailure ::
  HasCallStack =>
  (BuiltinData -> IO EvalResult) ->
  BuiltinData ->
  IO ()
expectFailure evaluateFunc contextData = do
  result <- evaluateFunc contextData
  result `shouldSatisfy` isEvaluationFailure

--------------------------------------------------------------------------------
-- Evaluation Result Predicates ------------------------------------------------

{- | Check if evaluation was successful

Predicate for testing EvalResult success in Hspec assertions.
-}
isEvaluationSuccess :: EvalResult -> Bool
isEvaluationSuccess EvalResult {evalResult = result} =
  case result of
    Left _ -> False
    Right _ -> True

{- | Check if evaluation failed

Predicate for testing EvalResult failure in Hspec assertions.
-}
isEvaluationFailure :: EvalResult -> Bool
isEvaluationFailure = not . isEvaluationSuccess

--------------------------------------------------------------------------------
-- Value Creation Helpers ------------------------------------------------------

{- | Convert Lovelace to Value for test scenarios

This helper function simplifies the creation of ADA-only values in tests,
converting from the semantic Lovelace type to the low-level Value type.
-}
lovelaceValue :: Lovelace -> Value
lovelaceValue (Lovelace n) = singleton adaSymbol adaToken n

{- | Convert ADA amount (in lovelace) to Value

This helper function creates ADA-only values directly from lovelace amounts,
making test values more readable. For example:
  adaValue 50_000_000 -- represents 50 ADA (with NumericUnderscores)
  adaValue 50000000  -- same, without underscores
-}
adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken
