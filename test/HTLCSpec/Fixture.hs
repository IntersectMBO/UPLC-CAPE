module HTLCSpec.Fixture (
  module HTLC.Fixture,

  -- * Transaction Data
  txId,
  txOutRef,
  txOutRef2,
  claimRedeemer,
  claimRedeemerWith,
  refundRedeemer,

  -- * Addresses
  impostorPubkey,
  payerAddr,
  recipientAddr,

  -- * Datum
  htlcDatum,

  -- * Value helpers
  lockedValue,
) where

import Prelude

import HTLC.Fixture
import PlutusLedgerApi.Data.V3
import ValidatorHelpers (lovelaceValue)

--------------------------------------------------------------------------------
-- Transaction Fixtures --------------------------------------------------------

txId :: TxId
txId = TxId "3333333333333333333333333333333333333333333333333333333333333333"

txOutRef :: TxOutRef
txOutRef = TxOutRef txId 0

txOutRef2 :: TxOutRef
txOutRef2 =
  TxOutRef
    (TxId "4444444444444444444444444444444444444444444444444444444444444444")
    0

claimRedeemer :: Redeemer
claimRedeemer = claimRedeemerWith correctPreimage

claimRedeemerWith :: BuiltinByteString -> Redeemer
claimRedeemerWith preimage = Redeemer (toBuiltinData (Claim preimage))

refundRedeemer :: Redeemer
refundRedeemer = Redeemer (toBuiltinData Refund)

--------------------------------------------------------------------------------
-- Address Fixtures ------------------------------------------------------------

impostorPubkey :: PubKeyHash
impostorPubkey =
  PubKeyHash "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

payerAddr :: Address
payerAddr = Address (PubKeyCredential payerKeyHash) Nothing

recipientAddr :: Address
recipientAddr = Address (PubKeyCredential recipientKeyHash) Nothing

--------------------------------------------------------------------------------
-- Datum -----------------------------------------------------------------------

htlcDatum :: Datum
htlcDatum =
  Datum . toBuiltinData $
    HTLCDatum
      { payer = payerAddr
      , recipient = recipientAddr
      , secretHash = secretHashBytes
      , timeout = timeoutPosix
      }

--------------------------------------------------------------------------------
-- Value Helpers ---------------------------------------------------------------

-- | Value locked at the HTLC script (2 ADA).
lockedValue :: Value
lockedValue = lovelaceValue (Lovelace 2_000_000)
