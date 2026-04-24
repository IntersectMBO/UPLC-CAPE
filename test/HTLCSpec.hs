{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module HTLCSpec (spec) where

import Prelude

import Cape.ScriptContextBuilder
import HTLC
import HTLCSpec.Fixture qualified as Fixed
import PlutusLedgerApi.Data.V3
import PlutusTx qualified
import Test.Hspec
import ValidatorHelpers

spec :: Spec
spec = do
  let validatorCode = $$(PlutusTx.compile [||htlcValidator||])
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

  describe "Claim" do
    it "succeeds well before timeout (time=50)" do
      let ctx = claimContext 50 Fixed.correctPreimage
      expectSuccess evaluateValidator ctx

    it "succeeds just before timeout (time=99)" do
      let ctx = claimContext 99 Fixed.correctPreimage
      expectSuccess evaluateValidator ctx

    it "fails at timeout boundary (time=100)" do
      let ctx = claimContext 100 Fixed.correctPreimage
      expectFailure evaluateValidator ctx

    it "fails after timeout (time=150)" do
      let ctx = claimContext 150 Fixed.correctPreimage
      expectFailure evaluateValidator ctx

    -- Exercises the exclusive-lowerBound branch of lowerBoundTime:
    -- LowerBound Finite 99 Exclusive is equivalent to inclusive time=100,
    -- which equals the timeout and must be rejected.
    it "fails with exclusive lower bound at timeout-1 (effective time=100)" do
      let range =
            Interval
              (LowerBound (Finite (POSIXTime 99)) False)
              (UpperBound PosInf True)
          ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.recipientKeyHash
                , SetValidRangeRaw range
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with non-finite (NegInf) lower bound" do
      let range =
            Interval
              (LowerBound NegInf True)
              (UpperBound PosInf True)
          ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.recipientKeyHash
                , SetValidRangeRaw range
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with wrong preimage" do
      let ctx = claimContext 50 Fixed.wrongPreimage
      expectFailure evaluateValidator ctx

    it "fails with empty preimage" do
      let ctx = claimContext 50 ""
      expectFailure evaluateValidator ctx

    it "fails without recipient signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , SetValidRange (Just (POSIXTime 50)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with payer signature instead of recipient" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.payerKeyHash
                , SetValidRange (Just (POSIXTime 50)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with impostor signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.impostorPubkey
                , SetValidRange (Just (POSIXTime 50)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with double satisfaction (two script inputs)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.recipientKeyHash
                , SetValidRange (Just (POSIXTime 50)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                , AddInputUTXO
                    Fixed.txOutRef2
                    Fixed.lockedValue
                    True
                    (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    -- An unrelated script input co-spent in the same tx must not trip the
    -- double-satisfaction guard. The spec pins the count to "exactly one
    -- input from *this* script address", not "exactly one script input
    -- overall" — otherwise the validator wrongly rejects legitimate
    -- multi-script transactions.
    it "succeeds when tx co-spends an unrelated script input" do
      let otherScriptAddr =
            Address
              ( ScriptCredential
                  (ScriptHash "2222222222222222222222222222222222222222222222222222222222")
              )
              Nothing
          otherScriptIn =
            TxInInfo
              ( TxOutRef
                  (TxId "5555555555555555555555555555555555555555555555555555555555555555")
                  0
              )
              ( TxOut otherScriptAddr (lovelaceValue (Lovelace 1_000_000)) NoOutputDatum Nothing
              )
          ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.claimRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.recipientKeyHash
                , SetValidRange (Just (POSIXTime 50)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                , AddInputUTXORaw otherScriptIn
                ]
      expectSuccess evaluateValidator ctx

  describe "Refund" do
    it "succeeds just after timeout (time=101)" do
      let ctx = refundContext 101
      expectSuccess evaluateValidator ctx

    -- Mirrors the claim's exclusive-lowerBound test:
    -- LowerBound Finite 100 Exclusive is equivalent to inclusive time=101,
    -- which is strictly after the timeout and must be accepted.
    it "succeeds with exclusive lower bound at timeout (effective time=101)" do
      let range =
            Interval
              (LowerBound (Finite (POSIXTime 100)) False)
              (UpperBound PosInf True)
          ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.payerKeyHash
                , SetValidRangeRaw range
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectSuccess evaluateValidator ctx

    it "succeeds well after timeout (time=200)" do
      let ctx = refundContext 200
      expectSuccess evaluateValidator ctx

    it "fails at timeout boundary (time=100)" do
      let ctx = refundContext 100
      expectFailure evaluateValidator ctx

    it "fails before timeout (time=50)" do
      let ctx = refundContext 50
      expectFailure evaluateValidator ctx

    it "fails without payer signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , SetValidRange (Just (POSIXTime 200)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with recipient signature instead of payer" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.recipientKeyHash
                , SetValidRange (Just (POSIXTime 200)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with impostor signature" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.impostorPubkey
                , SetValidRange (Just (POSIXTime 200)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails with double satisfaction (two script inputs)" do
      let ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum Fixed.htlcDatum
                , AddSignature Fixed.payerKeyHash
                , SetValidRange (Just (POSIXTime 200)) Nothing
                , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
                , AddInputUTXO
                    Fixed.txOutRef2
                    Fixed.lockedValue
                    True
                    (OutputDatum Fixed.htlcDatum)
                ]
      expectFailure evaluateValidator ctx

    it "fails when payer address is a script credential (not pubkey)" do
      let scriptPayerDatum =
            Datum . toBuiltinData $
              HTLCDatum
                { payer = Fixed.scriptAddr
                , recipient = Fixed.recipientAddr
                , secretHash = Fixed.secretHashBytes
                , timeout = Fixed.timeoutPosix
                }
          ctx =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum scriptPayerDatum
                , -- Add PubKeyHash "" as signer so that if extractPubKeyHash
                  -- silently returns "" the signature check passes
                  AddSignature (PubKeyHash "")
                , SetValidRange (Just (POSIXTime 200)) Nothing
                , AddInputUTXO
                    Fixed.txOutRef
                    Fixed.lockedValue
                    True
                    (OutputDatum scriptPayerDatum)
                ]
      expectFailure evaluateValidator ctx

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

-- | Build a Claim context with given time and preimage.
claimContext :: Integer -> BuiltinByteString -> BuiltinData
claimContext time preimage =
  buildContextData $
    ScriptContextBuilder
      SpendingBaseline
      [ SetRedeemer (Fixed.claimRedeemerWith preimage)
      , SetScriptDatum Fixed.htlcDatum
      , AddSignature Fixed.recipientKeyHash
      , SetValidRange (Just (POSIXTime time)) Nothing
      , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
      ]

-- | Build a Refund context with given time.
refundContext :: Integer -> BuiltinData
refundContext time =
  buildContextData $
    ScriptContextBuilder
      SpendingBaseline
      [ SetRedeemer Fixed.refundRedeemer
      , SetScriptDatum Fixed.htlcDatum
      , AddSignature Fixed.payerKeyHash
      , SetValidRange (Just (POSIXTime time)) Nothing
      , AddInputUTXO Fixed.txOutRef Fixed.lockedValue True (OutputDatum Fixed.htlcDatum)
      ]
