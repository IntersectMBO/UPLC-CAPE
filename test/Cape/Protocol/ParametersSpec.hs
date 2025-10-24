module Cape.Protocol.ParametersSpec (spec) where

import Cape.Protocol.Parameters (
  blockCpuBudget,
  blockMemoryBudget,
  executionFee,
  referenceScriptFee,
  scriptsPerBlock,
  scriptsPerTransaction,
  totalFee,
  txCpuBudget,
  txMemoryBudget,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

spec :: Spec
spec = do
  describe "Fee Calculations" $ do
    describe "executionFee" $ do
      it "calculates execution fee for small script (Example 1)" $ do
        let memUnits = 1_000_000
            cpuSteps = 500_000_000
            expected = 93_750
        executionFee memUnits cpuSteps `shouldBe` expected

      it "calculates execution fee for large script (Example 2)" $ do
        let memUnits = 1_000_000
            cpuSteps = 500_000_000
            expected = 93_750
        executionFee memUnits cpuSteps `shouldBe` expected

    describe "referenceScriptFee" $ do
      it "calculates reference script fee for 10 KB script (Tier 1)" $ do
        let scriptSize = 10_000 -- 10 KB
            expected = 150_000 -- 10,000 * 15
        referenceScriptFee scriptSize `shouldBe` expected

      it "calculates reference script fee for 75 KB script (Multi-Tier)" $ do
        let scriptSize = 75_000 -- 75 KB
        -- Tier 1 (0-25,600): 25,600 * 15 = 384,000
        -- Tier 2 (25,600-51,200): 25,600 * 18 = 460,800
        -- Tier 3 (51,200-75,000): 23,800 * 21.6 = 514,080
        -- Total: 1,358,880
            expected = 1_358_880
        referenceScriptFee scriptSize `shouldBe` expected

    describe "totalFee" $ do
      it "calculates total fee for small script (Example 1)" $ do
        let memUnits = 1_000_000
            cpuSteps = 500_000_000
            scriptSize = 10_000
            -- Execution: 93,750
            -- Reference: 150,000
            -- Total: 243,750
            expected = 243_750
        totalFee memUnits cpuSteps scriptSize `shouldBe` expected

      it "calculates total fee for large script (Example 2)" $ do
        let memUnits = 1_000_000
            cpuSteps = 500_000_000
            scriptSize = 75_000
            -- Execution: 93,750
            -- Reference: 1,358,880
            -- Total: 1,452,630
            expected = 1_452_630
        totalFee memUnits cpuSteps scriptSize `shouldBe` expected

  describe "Budget Utilization" $ do
    describe "Transaction Budget" $ do
      it "calculates tx memory budget percentage" $ do
        let memUnits = 1_000_000
            expected = 7.142857142857143 -- 1,000,000 / 14,000,000 * 100
            result = txMemoryBudget memUnits
            tolerance = 1e-10
        abs (result - expected) < tolerance `shouldBe` True

      it "calculates tx CPU budget percentage" $ do
        let cpuSteps = 500_000_000
            expected = 5.0 -- 500,000,000 / 10,000,000,000 * 100
        txCpuBudget cpuSteps `shouldBe` expected

    describe "Block Budget" $ do
      it "calculates block memory budget percentage" $ do
        let memUnits = 1_000_000
            expected = 1.6129032258064515 -- 1,000,000 / 62,000,000 * 100
        blockMemoryBudget memUnits `shouldBe` expected

      it "calculates block CPU budget percentage" $ do
        let cpuSteps = 500_000_000
            expected = 1.25 -- 500,000,000 / 40,000,000,000 * 100
        blockCpuBudget cpuSteps `shouldBe` expected

  describe "Capacity Calculations" $ do
    describe "scriptsPerTransaction" $ do
      it "calculates scripts per transaction (memory-limited)" $ do
        let memUnits = 1_000_000
            cpuSteps = 500_000_000
            expected = 14 -- min(14,000,000 / 1,000,000, 10,000,000,000 / 500,000,000)
        scriptsPerTransaction memUnits cpuSteps `shouldBe` expected

    describe "scriptsPerBlock" $ do
      it "calculates scripts per block (memory-limited)" $ do
        let memUnits = 1_000_000
            cpuSteps = 500_000_000
            expected = 62 -- min(62,000,000 / 1,000,000, 40,000,000,000 / 500,000,000)
        scriptsPerBlock memUnits cpuSteps `shouldBe` expected

  describe "Reference Script Fee Tier Boundaries" $ do
    it "calculates fee at tier 1 boundary (25,600 bytes)" $ do
      let scriptSize = 25_600
          expected = 384_000 -- Full tier 1
      referenceScriptFee scriptSize `shouldBe` expected

    it "calculates fee just after tier 1 boundary (25,601 bytes)" $ do
      let scriptSize = 25_601
          -- Tier 1: 25,600 * 15 = 384,000
          -- Tier 2: 1 * 18 = 18
          -- Total: 384,018
          expected = 384_018
      referenceScriptFee scriptSize `shouldBe` expected

    it "calculates fee at tier 2 boundary (51,200 bytes)" $ do
      let scriptSize = 51_200
          -- Tier 1: 384,000
          -- Tier 2: 25,600 * 18 = 460,800
          -- Total: 844,800
          expected = 844_800
      referenceScriptFee scriptSize `shouldBe` expected

    it "calculates fee at tier 3 boundary (76,800 bytes)" $ do
      let scriptSize = 76_800
          -- Tier 1: 384,000
          -- Tier 2: 460,800
          -- Tier 3: 25,600 * 21.6 = 552,960 (21.6 = 18 * 1.2)
          -- Total: 1,397,760
          expected = 1_397_760
      referenceScriptFee scriptSize `shouldBe` expected

  describe "Edge Cases" $ do
    it "handles zero memory and CPU" $ do
      executionFee 0 0 `shouldBe` 0
      txMemoryBudget 0 `shouldBe` 0.0
      txCpuBudget 0 `shouldBe` 0.0
      blockMemoryBudget 0 `shouldBe` 0.0
      blockCpuBudget 0 `shouldBe` 0.0

    it "handles zero script size" $ do
      referenceScriptFee 0 `shouldBe` 0

    it "handles minimal script size (1 byte)" $ do
      let scriptSize = 1
          expected = 15 -- 1 * 15
      referenceScriptFee scriptSize `shouldBe` expected

    it "handles very large CPU steps" $ do
      let cpuSteps = 10_000_000_000 -- Max transaction CPU
          expected = 100.0 -- 100% of transaction budget
      txCpuBudget cpuSteps `shouldBe` expected

    it "handles very large memory units" $ do
      let memUnits = 14_000_000 -- Max transaction memory
          expected = 100.0 -- 100% of transaction budget
      txMemoryBudget memUnits `shouldBe` expected
