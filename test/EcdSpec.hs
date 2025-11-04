module EcdSpec (spec) where

import Prelude

import Cape.PrettyResult (EvalResult (..), evaluateCompiledCode)
import Ecd
import PlutusCore.MkPlc (mkConstant)
import PlutusTx.Code (applyCode)
import PlutusTx.Lift (liftCodeDef)
import Test.Hspec

spec :: Spec
spec = do
  describe "ECD function tests" do
    it "ecd_0_12 should return 12 (edge case: first argument is zero)" do
      result <- evaluateEcd 0 12
      result `shouldSatisfy` isSuccessWithValue 12

    it "ecd_12_0 should return 12 (edge case: second argument is zero)" do
      result <- evaluateEcd 12 0
      result `shouldSatisfy` isSuccessWithValue 12

    it "ecd_7_7 should return 7 (edge case: identical numbers)" do
      result <- evaluateEcd 7 7
      result `shouldSatisfy` isSuccessWithValue 7

    it "ecd_6_9 should return 3" do
      result <- evaluateEcd 6 9
      result `shouldSatisfy` isSuccessWithValue 3

    it "ecd_12_8 should return 4" do
      result <- evaluateEcd 12 8
      result `shouldSatisfy` isSuccessWithValue 4

    it "ecd_15_25 should return 5" do
      result <- evaluateEcd 15 25
      result `shouldSatisfy` isSuccessWithValue 5

    it "ecd_17_19 should return 1 (coprime numbers)" do
      result <- evaluateEcd 17 19
      result `shouldSatisfy` isSuccessWithValue 1

    it "ecd_13_29 should return 1 (coprime numbers)" do
      result <- evaluateEcd 13 29
      result `shouldSatisfy` isSuccessWithValue 1

    it "ecd_48_18 should return 6" do
      result <- evaluateEcd 48 18
      result `shouldSatisfy` isSuccessWithValue 6

    it "ecd_100_75 should return 25" do
      result <- evaluateEcd 100 75
      result `shouldSatisfy` isSuccessWithValue 25

    it "ecd_1071_462 should return 21 (larger numbers)" do
      result <- evaluateEcd 1071 462
      result `shouldSatisfy` isSuccessWithValue 21

    it "ecd_2520_1890 should return 630 (larger numbers)" do
      result <- evaluateEcd 2520 1890
      result `shouldSatisfy` isSuccessWithValue 630

    it "ecd_negative_12_8 should return 4 (negative first argument)" do
      result <- evaluateEcd (-12) 8
      result `shouldSatisfy` isSuccessWithValue 4

    it "ecd_12_negative_8 should return 4 (negative second argument)" do
      result <- evaluateEcd 12 (-8)
      result `shouldSatisfy` isSuccessWithValue 4

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Evaluate ECD function with two integer inputs
evaluateEcd :: Integer -> Integer -> IO EvalResult
evaluateEcd a b = do
  case ecdCode `applyCode` liftCodeDef a of
    Left err -> error $ "Failed to apply first argument: " <> show err
    Right partiallyApplied ->
      case partiallyApplied `applyCode` liftCodeDef b of
        Left err -> error $ "Failed to apply second argument: " <> show err
        Right fullyApplied -> pure $ evaluateCompiledCode fullyApplied

-- | Check if evaluation succeeded with expected integer value
isSuccessWithValue :: Integer -> EvalResult -> Bool
isSuccessWithValue expected EvalResult {evalResult = result} =
  case result of
    Left _ -> False -- Evaluation failed
    Right term -> term == mkConstant () expected
