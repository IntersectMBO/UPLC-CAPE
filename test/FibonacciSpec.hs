module FibonacciSpec (spec) where

import Prelude

import Fibonacci
import PlutusCore.MkPlc (mkConstant)
import PlutusTx.Code (applyCode)
import PlutusTx.Eval (EvalResult (..), evaluateCompiledCode)
import PlutusTx.Lift (liftCodeDef)
import Test.Hspec

spec :: Spec
spec = do
  describe "Fibonacci function tests" do
    it "fibonacci_0 should return 0" do
      let input = 0
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 0

    it "fibonacci_1 should return 1" do
      let input = 1
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 1

    it "fibonacci_2 should return 1" do
      let input = 2
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 1

    it "fibonacci_3 should return 2" do
      let input = 3
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 2

    it "fibonacci_5 should return 5" do
      let input = 5
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 5

    it "fibonacci_8 should return 21" do
      let input = 8
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 21

    it "fibonacci_10 should return 55" do
      let input = 10
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 55

    it "fibonacci_15 should return 610" do
      let input = 15
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 610

    it "fibonacci_20 should return 6765" do
      let input = 20
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 6765

    it "fibonacci_25 should return 75025" do
      let input = 25
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue 75025

    it "fibonacci_negative should return -1" do
      let input = -1
      result <- evaluateFibonacci input
      result `shouldSatisfy` isSuccessWithValue (-1)

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Evaluate fibonacci function with given input
evaluateFibonacci :: Integer -> IO EvalResult
evaluateFibonacci input = do
  case fibonacciCode `applyCode` liftCodeDef input of
    Left err -> error $ "Failed to apply fibonacci code: " <> show err
    Right appliedCode -> pure $ evaluateCompiledCode appliedCode

-- | Check if evaluation succeeded with expected integer value
isSuccessWithValue :: Integer -> EvalResult -> Bool
isSuccessWithValue expected EvalResult {evalResult = result} =
  case result of
    Left _ -> False -- Evaluation failed
    Right term -> term == mkConstant () expected
