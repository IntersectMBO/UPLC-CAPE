{- | Aggregation of per-evaluation budget measurements into metrics.json

Every test case in a scenario is measured, and every measurement is recorded
in the @evaluations@ array of metrics.json. Aggregated metrics (sums, maxima,
medians — and the fees and budget percentages derived from them), however,
only cover evaluations whose test case is effectively included in aggregates:

* the test's @included_in_aggregates@ flag is @true@ (the default), and
* the test is not @pending@.

Excluded evaluations (attacks, malformed inputs, defensive checks — paths an
honest user's expected on-chain execution never exercises) are summarised
separately in the @measurements.excluded@ diagnostic block.

See @doc/adr/20260706-exclude-failure-path-evaluations-from-aggregated-metrics.md@.
-}
module Cape.Metrics (
  AggregationPolicy (..),
  EvaluationMetrics (..),
  BudgetAggregations (..),
  ExcludedAggregates (..),
  calculateBudgetAggregations,
  calculateExcludedAggregates,
  partitionByPolicy,
) where

import Prelude

import Data.Aeson (FromJSON (..), ToJSON (..), (.=))
import Data.Aeson qualified as Json
import Data.List (maximum, minimum, partition, (!!))

{- | Whether a test case's measured budget enters the aggregated metrics.

Decoded from the JSON boolean @included_in_aggregates@ (default @true@).
The measurement itself is always recorded in @evaluations@ either way.
-}
data AggregationPolicy = IncludedInAggregates | ExcludedFromAggregates
  deriving stock (Show, Eq)

instance FromJSON AggregationPolicy where
  parseJSON = Json.withBool "included_in_aggregates" \b ->
    pure if b then IncludedInAggregates else ExcludedFromAggregates

instance ToJSON AggregationPolicy where
  toJSON p = Json.Bool (p == IncludedInAggregates)

-- | Individual evaluation metrics
data EvaluationMetrics = EvaluationMetrics
  { evalName :: Text
  , evalDescription :: Text
  , evalCpuUnits :: Integer
  , evalMemoryUnits :: Integer
  , evalExecutionResult :: Text -- "success" or "error"
  , evalAggregationPolicy :: AggregationPolicy
  -- ^ Effective policy: the test's declared @included_in_aggregates@
  --   demoted to 'ExcludedFromAggregates' for pending tests.
  }

instance ToJSON EvaluationMetrics where
  toJSON (EvaluationMetrics name desc cpu mem result policy) =
    Json.object
      [ "name" .= name
      , "description" .= desc
      , "cpu_units" .= cpu
      , "memory_units" .= mem
      , "execution_result" .= result
      , "included_in_aggregates" .= policy
      ]

-- | Aggregated budget measurements over the included evaluations.
data BudgetAggregations = BudgetAggregations
  { aggMaximum :: Integer
  , aggSum :: Integer
  , aggMinimum :: Integer
  , aggMedian :: Integer
  }

instance ToJSON BudgetAggregations where
  toJSON (BudgetAggregations maxVal sumVal minVal medVal) =
    Json.object
      [ "maximum" .= maxVal
      , "sum" .= sumVal
      , "minimum" .= minVal
      , "median" .= medVal
      ]

{- | Diagnostic totals for evaluations excluded from aggregates.
Always emitted; 'exclCount' is 0 when nothing is excluded.
-}
data ExcludedAggregates = ExcludedAggregates
  { exclCount :: Int
  , exclCpuSum :: Integer
  , exclCpuMaximum :: Integer
  , exclMemSum :: Integer
  , exclMemMaximum :: Integer
  }

instance ToJSON ExcludedAggregates where
  toJSON (ExcludedAggregates count cpuSum cpuMax memSum memMax) =
    Json.object
      [ "count" .= count
      , "cpu_units" .= Json.object ["sum" .= cpuSum, "maximum" .= cpuMax]
      , "memory_units" .= Json.object ["sum" .= memSum, "maximum" .= memMax]
      ]

-- | Split evaluations into (included, excluded) by their effective policy.
partitionByPolicy ::
  [EvaluationMetrics] -> ([EvaluationMetrics], [EvaluationMetrics])
partitionByPolicy =
  partition ((== IncludedInAggregates) . evalAggregationPolicy)

-- | Calculate aggregations for a list of budget values
calculateBudgetAggregations :: [Integer] -> BudgetAggregations
calculateBudgetAggregations values =
  let sortedValues = sort values
      median = calculateMedian sortedValues
   in BudgetAggregations
        { aggMaximum = if null values then 0 else maximum values
        , aggSum = sum values
        , aggMinimum = if null values then 0 else minimum values
        , aggMedian = median
        }

-- | Summarise the excluded evaluations for the diagnostic block.
calculateExcludedAggregates :: [EvaluationMetrics] -> ExcludedAggregates
calculateExcludedAggregates evals =
  let cpus = map evalCpuUnits evals
      mems = map evalMemoryUnits evals
   in ExcludedAggregates
        { exclCount = length evals
        , exclCpuSum = sum cpus
        , exclCpuMaximum = if null cpus then 0 else maximum cpus
        , exclMemSum = sum mems
        , exclMemMaximum = if null mems then 0 else maximum mems
        }

-- | Calculate median from a sorted list
calculateMedian :: [Integer] -> Integer
calculateMedian [] = 0
calculateMedian xs =
  let len = length xs
      mid = len `div` 2
   in if even len
        then (xs !! (mid - 1) + xs !! mid) `div` 2
        else xs !! mid
