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

  -- ** Transaction Data
  txId,
  txOutRef,
  depositRedeemer,
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

{- | Deposit action redeemer for testing

The deposit redeemer (integer 0) used consistently across ScriptContext tests.
Corresponds to the deposit action in the two-party escrow validator logic.
-}
depositRedeemer :: V3.Redeemer
depositRedeemer = V3.Redeemer (V3.toBuiltinData (0 :: Integer))
