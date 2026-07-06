module Cape.MetricsSpec (spec) where

import Prelude

import Cape.Metrics
import Test.Hspec

-- | Helper: an evaluation with the given budgets and aggregation policy.
mkEval :: Text -> Integer -> Integer -> AggregationPolicy -> EvaluationMetrics
mkEval name cpu mem policy =
  EvaluationMetrics
    { evalName = name
    , evalDescription = ""
    , evalCpuUnits = cpu
    , evalMemoryUnits = mem
    , evalExecutionResult = "success"
    , evalAggregationPolicy = policy
    }

spec :: Spec
spec = do
  describe "calculateBudgetAggregations" do
    it "returns zeros for an empty list" do
      let agg = calculateBudgetAggregations []
      aggMaximum agg `shouldBe` 0
      aggSum agg `shouldBe` 0
      aggMinimum agg `shouldBe` 0
      aggMedian agg `shouldBe` 0

    it "computes sum, maximum, minimum and median (odd length)" do
      let agg = calculateBudgetAggregations [30, 10, 20]
      aggSum agg `shouldBe` 60
      aggMaximum agg `shouldBe` 30
      aggMinimum agg `shouldBe` 10
      aggMedian agg `shouldBe` 20

    it "computes median as mean of middle values (even length)" do
      let agg = calculateBudgetAggregations [40, 10, 20, 30]
      aggMedian agg `shouldBe` 25

  describe "partitionByPolicy" do
    it "splits evaluations into included and excluded" do
      let a = mkEval "a" 1 1 IncludedInAggregates
          b = mkEval "b" 2 2 ExcludedFromAggregates
          c = mkEval "c" 3 3 IncludedInAggregates
          (included, excluded) = partitionByPolicy [a, b, c]
      map evalName included `shouldBe` ["a", "c"]
      map evalName excluded `shouldBe` ["b"]

  describe "calculateExcludedAggregates" do
    it "summarises an empty exclusion set as zeros" do
      let excl = calculateExcludedAggregates []
      exclCount excl `shouldBe` 0
      exclCpuSum excl `shouldBe` 0
      exclCpuMaximum excl `shouldBe` 0
      exclMemSum excl `shouldBe` 0
      exclMemMaximum excl `shouldBe` 0

    it "sums and maximises cpu/memory over excluded evaluations only" do
      let evals =
            [ mkEval "x" 100 10 ExcludedFromAggregates
            , mkEval "y" 300 5 ExcludedFromAggregates
            ]
          excl = calculateExcludedAggregates evals
      exclCount excl `shouldBe` 2
      exclCpuSum excl `shouldBe` 400
      exclCpuMaximum excl `shouldBe` 300
      exclMemSum excl `shouldBe` 15
      exclMemMaximum excl `shouldBe` 10

    it "leaves included aggregates unaffected by excluded values" do
      let a = mkEval "a" 10 1 IncludedInAggregates
          b = mkEval "b" 9999 9999 ExcludedFromAggregates
          (included, _) = partitionByPolicy [a, b]
          agg = calculateBudgetAggregations (map evalCpuUnits included)
      aggSum agg `shouldBe` 10
      aggMaximum agg `shouldBe` 10
