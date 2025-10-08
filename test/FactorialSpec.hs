module FactorialSpec (spec) where

import Prelude

import Cape.PrettyResult (EvalResult (..), evaluateCompiledCode)
import Factorial
import PlutusCore.MkPlc (mkConstant)
import PlutusTx.Code (applyCode)
import PlutusTx.Lift (liftCodeDef)
import Test.Hspec

spec :: Spec
spec = do
  describe "Factorial function tests" do
    it "factorial_0 should return 1" do
      let input = 0
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 1

    it "factorial_1 should return 1" do
      let input = 1
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 1

    it "factorial_2 should return 2" do
      let input = 2
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 2

    it "factorial_3 should return 6" do
      let input = 3
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 6

    it "factorial_4 should return 24" do
      let input = 4
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 24

    it "factorial_5 should return 120" do
      let input = 5
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 120

    it "factorial_8 should return 40320" do
      let input = 8
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 40320

    it "factorial_10 should return 3628800" do
      let input = 10
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 3628800

    it "factorial_12 should return 479001600" do
      let input = 12
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 479001600

    it "factorial_negative should return 1" do
      let input = -5
      result <- evaluateFactorial input
      result `shouldSatisfy` isSuccessWithValue 1

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Evaluate factorial function with given input
evaluateFactorial :: Integer -> IO EvalResult
evaluateFactorial input = do
  case factorialCode `applyCode` liftCodeDef input of
    Left err -> error $ "Failed to apply factorial code: " <> show err
    Right appliedCode -> pure $ evaluateCompiledCode appliedCode

-- | Check if evaluation succeeded with expected integer value
isSuccessWithValue :: Integer -> EvalResult -> Bool
isSuccessWithValue expected EvalResult {evalResult = result} =
  case result of
    Left _ -> False -- Evaluation failed
    Right term -> term == mkConstant () expected
