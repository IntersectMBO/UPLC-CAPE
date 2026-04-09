module LinearVestingSpec.Fixture (
  module LinearVesting.Fixture,

  -- * Transaction Data
  txId,
  txOutRef,
  txOutRef2,
  partialUnlockRedeemer,
  fullUnlockRedeemer,

  -- * Addresses
  impostorPubkey,
  beneficiaryAddr,

  -- * Datum
  vestingDatum,

  -- * Value helpers
  vestingValue,
) where

import Prelude

import LinearVesting.Fixture
import PlutusLedgerApi.Data.V3
import ValidatorHelpers (lovelaceValue)

--------------------------------------------------------------------------------
-- Transaction Fixtures --------------------------------------------------------

txId :: TxId
txId = TxId "3333333333333333333333333333333333333333333333333333333333333333"

txOutRef :: TxOutRef
txOutRef = TxOutRef txId 0

txOutRef2 :: TxOutRef
txOutRef2 = TxOutRef txId 1

partialUnlockRedeemer :: Redeemer
partialUnlockRedeemer = Redeemer (toBuiltinData PartialUnlock)

fullUnlockRedeemer :: Redeemer
fullUnlockRedeemer = Redeemer (toBuiltinData FullUnlock)

--------------------------------------------------------------------------------
-- Address Fixtures ------------------------------------------------------------

impostorPubkey :: PubKeyHash
impostorPubkey =
  PubKeyHash "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

beneficiaryAddr :: Address
beneficiaryAddr = Address (PubKeyCredential beneficiaryKeyHash) Nothing

--------------------------------------------------------------------------------
-- Datum -----------------------------------------------------------------------

vestingDatum :: Datum
vestingDatum =
  Datum . toBuiltinData $
    VestingDatum
      { beneficiary = beneficiaryAddr
      , vestingAsset = (vestingCurrencySymbol, vestingTokenName)
      , totalVestingQty = 1000
      , vestingPeriodStart = 0
      , vestingPeriodEnd = 100
      , firstUnlockPossibleAfter = 10
      , totalInstallments = 10
      }

--------------------------------------------------------------------------------
-- Value Helpers ---------------------------------------------------------------

-- | Create a Value with lovelace + vesting tokens
vestingValue :: Integer -> Value
vestingValue tokenQty =
  lovelaceValue (Lovelace 2_000_000)
    <> singleton vestingCurrencySymbol vestingTokenName tokenQty
