{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module LinearVestingSpec (spec) where

import Prelude

import Cape.ScriptContextBuilder
import LinearVesting
import LinearVestingSpec.Fixture qualified as Fixed
import PlutusLedgerApi.Data.V3
import PlutusTx qualified
import Test.Hspec
import ValidatorHelpers

spec :: Spec
spec = do
  let validatorCode = $$(PlutusTx.compile [||linearVestingValidator||])
      evaluateValidator = evaluateValidatorCode validatorCode

  describe "Invalid redeemer types" do
    it "fails with integer redeemer 0" do
      let invalidData = toBuiltinData (0 :: Integer)
      expectFailure evaluateValidator invalidData

    it "fails with integer redeemer 1" do
      let invalidData = toBuiltinData (1 :: Integer)
      expectFailure evaluateValidator invalidData

    it "fails with bytestring redeemer" do
      let invalidData = toBuiltinData ("deadbeef" :: BuiltinByteString)
      expectFailure evaluateValidator invalidData

  describe "PartialUnlock" do
    it "first installment succeeds (time=11, 1000→900)" do
      let ctx = partialUnlockContext 11 1000 900
      expectSuccess evaluateValidator ctx

    it "mid-vesting succeeds (time=50, 1000→500)" do
      let ctx = partialUnlockContext 50 1000 500
      expectSuccess evaluateValidator ctx

    it "near end succeeds (time=91, 1000→100)" do
      let ctx = partialUnlockContext 91 1000 100
      expectSuccess evaluateValidator ctx

    it "between installments succeeds (time=25, 1000→800)" do
      let ctx = partialUnlockContext 25 1000 800
      expectSuccess evaluateValidator ctx

    it "fails without beneficiary signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.partialUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , -- no signature
                  SetValidRange (Just (POSIXTime 11)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue 900) Fixed.vestingDatum
                ]
      expectFailure evaluateValidator ctx

    it "fails with wrong signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.partialUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.impostorPubkey
                , SetValidRange (Just (POSIXTime 11)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue 900) Fixed.vestingDatum
                ]
      expectFailure evaluateValidator ctx

    it "fails before firstUnlockPossibleAfter (time=5)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.partialUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 5)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue 900) Fixed.vestingDatum
                ]
      expectFailure evaluateValidator ctx

    it "fails at exact boundary (time=10)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.partialUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 10)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue 900) Fixed.vestingDatum
                ]
      expectFailure evaluateValidator ctx

    it "fails with wrong remaining amount (too low)" do
      let ctx = partialUnlockContext' 11 1000 800 -- should be 900
      expectFailure evaluateValidator ctx

    it "fails with wrong remaining amount (too high)" do
      let ctx = partialUnlockContext' 11 1000 950 -- should be 900
      expectFailure evaluateValidator ctx

    it "fails with zero remaining" do
      let ctx = partialUnlockContext' 11 1000 0
      expectFailure evaluateValidator ctx

    it "fails when remaining not decreasing" do
      let ctx = partialUnlockContext' 11 1000 1000
      expectFailure evaluateValidator ctx

    it "fails with double satisfaction (two script inputs)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.partialUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 11)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddInputUTXO Fixed.txOutRef2 (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue 900) Fixed.vestingDatum
                ]
      expectFailure evaluateValidator ctx

    it "fails with infinite lower bound (no from_time)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.partialUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange Nothing (Just (POSIXTime 90))
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue 900) Fixed.vestingDatum
                ]
      expectFailure evaluateValidator ctx

  describe "FullUnlock" do
    it "succeeds after vesting period (time=101)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.fullUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 101)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                ]
      expectSuccess evaluateValidator ctx

    it "succeeds well after period (time=200)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.fullUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 200)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                ]
      expectSuccess evaluateValidator ctx

    it "fails without beneficiary signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.fullUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , SetValidRange (Just (POSIXTime 101)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails before vesting period end (time=50)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.fullUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 50)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails at exact boundary (time=100)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.fullUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange (Just (POSIXTime 100)) Nothing
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with infinite lower bound (no from_time)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.fullUnlockRedeemer
                , SetScriptDatum Fixed.vestingDatum
                , AddSignature Fixed.beneficiaryKeyHash
                , SetValidRange Nothing (Just (POSIXTime 200))
                , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue 1000) True (OutputDatum Fixed.vestingDatum)
                ]
      expectFailure evaluateValidator ctx

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

-- | Build a successful PartialUnlock context
partialUnlockContext :: Integer -> Integer -> Integer -> BuiltinData
partialUnlockContext = partialUnlockContext'

-- | Build a PartialUnlock context (may fail if amounts are wrong)
partialUnlockContext' :: Integer -> Integer -> Integer -> BuiltinData
partialUnlockContext' time inputTokens outputTokens =
  buildContextData $
    ScriptContextBuilder
      SpendingBaseline
      [ SetRedeemer Fixed.partialUnlockRedeemer
      , SetScriptDatum Fixed.vestingDatum
      , AddSignature Fixed.beneficiaryKeyHash
      , SetValidRange (Just (POSIXTime time)) Nothing
      , AddInputUTXO Fixed.txOutRef (Fixed.vestingValue inputTokens) True (OutputDatum Fixed.vestingDatum)
      , AddOutputUTXOWithDatum Fixed.scriptAddr (Fixed.vestingValue outputTokens) Fixed.vestingDatum
      ]
