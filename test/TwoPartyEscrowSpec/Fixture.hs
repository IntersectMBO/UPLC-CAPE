{- | Test fixture data for TwoPartyEscrowSpec

This module re-exports all validator fixtures from TwoPartyEscrow.Fixture
and adds spec-only fixtures for testing purposes. Import qualified as 'Fixed'
for readable fixture references like 'Fixed.scriptAddr'.
-}
module TwoPartyEscrowSpec.Fixture (
  -- * Re-exported Validator Fixtures
  module TwoPartyEscrow.Fixture,

  -- * Spec-Only Fixtures

  -- ** Addresses (scriptAddr now from main fixture)
  impostorPubkey,
  impostorAddr,
  sellerAddr,

  -- ** Transaction Data
  txId,
  txOutRef,
  txOutRef2,
  depositRedeemer,
  acceptRedeemer,
  refundRedeemer,

  -- ** Addresses
  buyerAddr,

  -- ** Helper Functions (re-exported from ValidatorHelpers)
  lovelaceValue,
  adaValue,

  -- ** Datum Helpers
  depositedEscrowDatum,
  acceptedEscrowDatum,
  refundedEscrowDatum,
) where

import Prelude

import PlutusLedgerApi.Data.V3
import TwoPartyEscrow.Fixture
import ValidatorHelpers (adaValue, lovelaceValue)

--------------------------------------------------------------------------------
-- Spec-Only Fixtures ---------------------------------------------------------

-- ** Address Fixtures

{- | Script address is now exported from TwoPartyEscrow.Fixture
   using the unified PlutusLedgerApi.Data.V3 Address type
-}

{- | Standardized impostor pubkey hash for testing wrong address scenarios

This pubkey hash (all c's) is used to test scenarios where funds are
incorrectly sent to an impostor's address instead of the script.
-}
impostorPubkey :: PubKeyHash
impostorPubkey =
  PubKeyHash "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

{- | Impostor address for testing wrong address scenarios

This address uses the standardized impostor pubkey hash (all c's)
for testing scenarios where payments are incorrectly sent to an impostor.
-}
impostorAddr :: Address
impostorAddr = Address (PubKeyCredential impostorPubkey) Nothing

{- | Seller address for testing Accept operation scenarios

This address uses the seller's pubkey hash for Accept operation tests
where payments should be made to the seller.
-}
sellerAddr :: Address
sellerAddr = Address (PubKeyCredential sellerKeyHash) Nothing

{- | Buyer address for testing Refund operation scenarios

This address uses the buyer's pubkey hash for Refund operation tests
where funds should be returned to the buyer.
-}
buyerAddr :: Address
buyerAddr = Address (PubKeyCredential buyerKeyHash) Nothing

-- ** Transaction Fixtures

{- | Standard transaction ID for testing (matches cape-tests.json)

This transaction ID (all 3's) is used consistently across all ScriptContext
tests to ensure reproducible test scenarios that align with JSON test data.
-}
txId :: TxId
txId = TxId "3333333333333333333333333333333333333333333333333333333333333333"

{- | Standard UTXO reference for testing

Uses the standardized txId with output index 0, matching the pattern
used consistently across all ScriptContext tests and cape-tests.json.
-}
txOutRef :: TxOutRef
txOutRef = TxOutRef txId 0

{- | Second UTXO reference for Accept operation testing

Uses the same standardized txId with output index 1, simulating
the second transaction in the Accept sequence after Deposit.
-}
txOutRef2 :: TxOutRef
txOutRef2 = TxOutRef txId 1

{- | Deposit action redeemer for testing

The deposit redeemer (integer 0) used consistently across ScriptContext tests.
Corresponds to the deposit action in the two-party escrow validator logic.
-}
depositRedeemer :: Redeemer
depositRedeemer = Redeemer (toBuiltinData (0 :: Integer))

{- | Accept action redeemer for testing

The accept redeemer (integer 1) used for Accept operation tests.
Corresponds to the accept action in the two-party escrow validator logic.
-}
acceptRedeemer :: Redeemer
acceptRedeemer = Redeemer (toBuiltinData (1 :: Integer))

{- | Refund action redeemer for testing

The refund redeemer (integer 2) used for Refund operation tests.
Corresponds to the refund action in the two-party escrow validator logic.
-}
refundRedeemer :: Redeemer
refundRedeemer = Redeemer (toBuiltinData (2 :: Integer))

--------------------------------------------------------------------------------
-- Datum Helpers ---------------------------------------------------------------

{- | Create a Deposited state escrow datum for testing

Standard datum representing an escrow that has been deposited but not yet
accepted or refunded. Uses a fixed deposit time of 1000 seconds for consistent
test scenarios.
-}
depositedEscrowDatum :: Datum
depositedEscrowDatum =
  let escrowDatum =
        EscrowDatum
          { escrowState = Deposited
          , depositTime = POSIXTime 1000 -- Fixed deposit time for testing
          }
   in Datum (toBuiltinData escrowDatum)

{- | Create an Accepted state escrow datum for testing

Datum representing an escrow that has been accepted by the seller.
This is used to test invalid state transitions (e.g., refund after accept).
-}
acceptedEscrowDatum :: Datum
acceptedEscrowDatum =
  let escrowDatum =
        EscrowDatum
          { escrowState = Accepted
          , depositTime = POSIXTime 1000
          }
   in Datum (toBuiltinData escrowDatum)

{- | Create a Refunded state escrow datum for testing

Datum representing an escrow that has been refunded to the buyer.
This is used to test invalid state transitions (e.g., accept after refund).
-}
refundedEscrowDatum :: Datum
refundedEscrowDatum =
  let escrowDatum =
        EscrowDatum
          { escrowState = Refunded
          , depositTime = POSIXTime 1000
          }
   in Datum (toBuiltinData escrowDatum)

-- ** Helper Functions are re-exported from ValidatorHelpers
