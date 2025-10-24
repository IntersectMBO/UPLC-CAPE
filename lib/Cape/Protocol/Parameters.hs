{- | Conway Era Mainnet Protocol Parameters

This module provides protocol parameters and derived metric calculations
for the UPLC-CAPE benchmarking framework.

For comprehensive documentation including formulas, examples, and
interpretation guidelines, see: doc/metrics.md
-}
module Cape.Protocol.Parameters (
  -- * Conway Era Mainnet Protocol Parameters

  -- ** Transaction Fee Parameters
  txFeePerByte,
  txFeeFixed,

  -- ** Plutus Execution Unit Prices
  priceMemory,
  priceSteps,

  -- ** Execution Unit Limits (Per Transaction)
  maxTxMemory,
  maxTxCpu,

  -- ** Execution Unit Limits (Per Block)
  maxBlockMemory,
  maxBlockCpu,

  -- ** Reference Script Fee Parameters (Conway Era)
  minFeeRefScriptCostPerByte,
  refScriptTierSize,
  refScriptTierMultiplier,

  -- * Fee Calculations
  executionFee,
  referenceScriptFee,
  totalFee,

  -- * Budget Utilization
  txMemoryBudget,
  txCpuBudget,
  blockMemoryBudget,
  blockCpuBudget,

  -- * Capacity Calculations
  scriptsPerTransaction,
  scriptsPerBlock,
) where

import Data.Ratio ((%))
import Prelude

--------------------------------------------------------------------------------
-- Transaction Fee Parameters --------------------------------------------------

-- | Base transaction fee per byte (lovelace/byte)
txFeePerByte :: Integer
txFeePerByte = 44

-- | Fixed transaction fee component (lovelace)
txFeeFixed :: Integer
txFeeFixed = 155_381

--------------------------------------------------------------------------------
-- Plutus Execution Unit Prices ------------------------------------------------

-- | Price per memory unit (lovelace/memory unit)
priceMemory :: Rational
priceMemory = 577 % 10_000 -- 0.0577

-- | Price per CPU step (lovelace/CPU step)
priceSteps :: Rational
priceSteps = 721 % 10_000_000 -- 0.0000721

--------------------------------------------------------------------------------
-- Execution Unit Limits (Per Transaction) -------------------------------------

-- | Maximum memory units per transaction
maxTxMemory :: Integer
maxTxMemory = 14_000_000

-- | Maximum CPU steps per transaction
maxTxCpu :: Integer
maxTxCpu = 10_000_000_000

--------------------------------------------------------------------------------
-- Execution Unit Limits (Per Block) -------------------------------------------

-- | Maximum memory units per block
maxBlockMemory :: Integer
maxBlockMemory = 62_000_000

-- | Maximum CPU steps per block
maxBlockCpu :: Integer
maxBlockCpu = 40_000_000_000

--------------------------------------------------------------------------------
-- Reference Script Fee Parameters (Conway Era) --------------------------------

-- | Minimum fee per byte for reference scripts (first tier, lovelace/byte)
minFeeRefScriptCostPerByte :: Integer
minFeeRefScriptCostPerByte = 15

-- | Size of each tier in reference script fee calculation (25 KiB)
refScriptTierSize :: Integer
refScriptTierSize = 25_600

-- | Multiplier for each subsequent tier (1.2)
refScriptTierMultiplier :: Rational
refScriptTierMultiplier = 6 % 5

--------------------------------------------------------------------------------
-- Fee Calculations ------------------------------------------------------------

{- | Calculate execution fee for a script

Formula: ceiling((memUnits * priceMemory) + (cpuSteps * priceSteps))

This represents the cost of executing the script, independent of its size.
-}
executionFee :: Integer -> Integer -> Integer
executionFee memUnits cpuSteps =
  ceiling
    ( (fromIntegral memUnits * priceMemory)
        + (fromIntegral cpuSteps * priceSteps)
    )

{- | Calculate reference script fee using tiered pricing

The fee increases in tiers of 25 KiB, with each tier costing 1.2x the
previous tier. This is the Conway era reference script fee structure.

Example tiers:
  Tier 1 (0-25 KiB):    15 lovelace/byte
  Tier 2 (25-50 KiB):   18 lovelace/byte
  Tier 3 (50-75 KiB):   21.6 lovelace/byte
  ... and so on
-}
referenceScriptFee :: Integer -> Integer
referenceScriptFee scriptSize =
  floor $ go 0 (fromIntegral minFeeRefScriptCostPerByte) scriptSize
  where
    go :: Rational -> Rational -> Integer -> Rational
    go acc curPrice remaining
      | remaining < refScriptTierSize =
          acc + (fromIntegral remaining * curPrice)
      | otherwise =
          let acc' = acc + (curPrice * fromIntegral refScriptTierSize)
              nextPrice = curPrice * refScriptTierMultiplier
           in go acc' nextPrice (remaining - refScriptTierSize)

{- | Calculate total fee for a script used as a reference script

This includes both the execution cost and the reference script storage cost.
Note: This does NOT include the base transaction fee (txFeeFixed +
txFeePerByte), as we're comparing script performance, not transaction costs.
-}
totalFee :: Integer -> Integer -> Integer -> Integer
totalFee memUnits cpuSteps scriptSize =
  executionFee memUnits cpuSteps + referenceScriptFee scriptSize

--------------------------------------------------------------------------------
-- Budget Utilization ----------------------------------------------------------

-- | Calculate percentage of transaction memory budget used
txMemoryBudget :: Integer -> Double
txMemoryBudget memUnits =
  (fromIntegral memUnits / fromIntegral maxTxMemory) * 100

-- | Calculate percentage of transaction CPU budget used
txCpuBudget :: Integer -> Double
txCpuBudget cpuSteps =
  (fromIntegral cpuSteps / fromIntegral maxTxCpu) * 100

-- | Calculate percentage of block memory budget used
blockMemoryBudget :: Integer -> Double
blockMemoryBudget memUnits =
  (fromIntegral memUnits / fromIntegral maxBlockMemory) * 100

-- | Calculate percentage of block CPU budget used
blockCpuBudget :: Integer -> Double
blockCpuBudget cpuSteps =
  (fromIntegral cpuSteps / fromIntegral maxBlockCpu) * 100

--------------------------------------------------------------------------------
-- Capacity Calculations -------------------------------------------------------

{- | Calculate how many script executions can fit in a single transaction

This is limited by whichever resource (memory or CPU) runs out first.
-}
scriptsPerTransaction :: Integer -> Integer -> Integer
scriptsPerTransaction memUnits cpuSteps =
  min
    (maxTxMemory `div` memUnits)
    (maxTxCpu `div` cpuSteps)

{- | Calculate how many script executions can fit in a single block

This is limited by whichever resource (memory or CPU) runs out first.
-}
scriptsPerBlock :: Integer -> Integer -> Integer
scriptsPerBlock memUnits cpuSteps =
  min
    (maxBlockMemory `div` memUnits)
    (maxBlockCpu `div` cpuSteps)
