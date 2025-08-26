{- | Test fixture data for TwoPartyEscrowSpec

This module re-exports all validator fixtures from TwoPartyEscrow.Fixture
and adds spec-only fixtures for testing purposes. Import qualified as 'Fixed'
for readable fixture references like 'Fixed.scriptAddr'.
-}
module TwoPartyEscrowSpec.Fixture (
  -- * Re-exported Validator Fixtures
  module TwoPartyEscrow.Fixture,

  -- * Spec-Only Fixtures

  -- ** Addresses
  scriptAddr,
  impostorPubkey,
  impostorAddr,
  sellerAddr,

  -- ** Transaction Data
  txId,
  txOutRef,
  txOutRef2,
  depositRedeemer,
  acceptRedeemer,
) where

import Prelude

import PlutusLedgerApi.V3 qualified as V3
import TwoPartyEscrow.Fixture

--------------------------------------------------------------------------------
-- Spec-Only Fixtures ---------------------------------------------------------

-- ** Address Fixtures

{- | Standard script address for testing (matches ScriptContextBuilder)

This address uses the standardized script hash (all 1's) that matches
the script address used in ScriptContextBuilder for consistent testing.
-}
scriptAddr :: V3.Address
scriptAddr =
  V3.Address
    ( V3.ScriptCredential
        (V3.ScriptHash "1111111111111111111111111111111111111111111111111111111111")
    )
    Nothing

{- | Standardized impostor pubkey hash for testing wrong address scenarios

This pubkey hash (all c's) is used to test scenarios where funds are
incorrectly sent to an impostor's address instead of the script.
-}
impostorPubkey :: V3.PubKeyHash
impostorPubkey =
  V3.PubKeyHash "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

{- | Impostor address for testing wrong address scenarios

This address uses the standardized impostor pubkey hash (all c's)
for testing scenarios where payments are incorrectly sent to an impostor.
-}
impostorAddr :: V3.Address
impostorAddr = V3.Address (V3.PubKeyCredential impostorPubkey) Nothing

{- | Seller address for testing Accept operation scenarios

This address uses the seller's pubkey hash for Accept operation tests
where payments should be made to the seller.
-}
sellerAddr :: V3.Address
sellerAddr = V3.Address (V3.PubKeyCredential sellerKeyHash) Nothing

-- ** Transaction Fixtures

{- | Standard transaction ID for testing (matches cape-tests.json)

This transaction ID (all 3's) is used consistently across all ScriptContext
tests to ensure reproducible test scenarios that align with JSON test data.
-}
txId :: V3.TxId
txId = V3.TxId "3333333333333333333333333333333333333333333333333333333333333333"

{- | Standard UTXO reference for testing

Uses the standardized txId with output index 0, matching the pattern
used consistently across all ScriptContext tests and cape-tests.json.
-}
txOutRef :: V3.TxOutRef
txOutRef = V3.TxOutRef txId 0

{- | Second UTXO reference for Accept operation testing

Uses the same standardized txId with output index 1, simulating
the second transaction in the Accept sequence after Deposit.
-}
txOutRef2 :: V3.TxOutRef
txOutRef2 = V3.TxOutRef txId 1

{- | Deposit action redeemer for testing

The deposit redeemer (integer 0) used consistently across ScriptContext tests.
Corresponds to the deposit action in the two-party escrow validator logic.
-}
depositRedeemer :: V3.Redeemer
depositRedeemer = V3.Redeemer (V3.toBuiltinData (0 :: Integer))

{- | Accept action redeemer for testing

The accept redeemer (integer 1) used for Accept operation tests.
Corresponds to the accept action in the two-party escrow validator logic.
-}
acceptRedeemer :: V3.Redeemer
acceptRedeemer = V3.Redeemer (V3.toBuiltinData (1 :: Integer))
