{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- | Test fixture data for LinearVesting benchmark
module LinearVesting.Fixture (
  -- * Datum and Redeemer Types
  VestingDatum (..),
  VestingRedeemer (..),

  -- * Beneficiary Fixture Data
  beneficiaryKeyHash,
  beneficiaryKeyHashBytes,

  -- * Vesting Asset
  vestingCurrencySymbol,
  vestingTokenName,

  -- * Script Address
  scriptAddr,
) where

import PlutusLedgerApi.Data.V3
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Prelude

--------------------------------------------------------------------------------
-- Datum and Redeemer Types ----------------------------------------------------

-- | Vesting parameters stored on-chain as inline datum
data VestingDatum = VestingDatum
  { beneficiary :: Address
  -- ^ Address of the beneficiary (must sign withdrawal transactions)
  , vestingAsset :: (CurrencySymbol, TokenName)
  -- ^ The asset being vested (currency symbol + token name)
  , totalVestingQty :: Integer
  -- ^ Total quantity of tokens being vested
  , vestingPeriodStart :: Integer
  -- ^ POSIX timestamp when vesting period begins
  , vestingPeriodEnd :: Integer
  -- ^ POSIX timestamp when vesting period ends (full unlock available)
  , firstUnlockPossibleAfter :: Integer
  -- ^ POSIX timestamp before which no partial unlock is allowed
  , totalInstallments :: Integer
  -- ^ Number of equal installments to divide vesting period into
  }

-- | Redeemer actions for the vesting validator
data VestingRedeemer
  = -- | Withdraw proportional tokens during vesting period
    PartialUnlock
  | -- | Withdraw all remaining tokens after vesting period ends
    FullUnlock

-- PlutusTx instances for serialization
makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]
makeIsDataIndexed ''VestingRedeemer [('PartialUnlock, 0), ('FullUnlock, 1)]

--------------------------------------------------------------------------------
-- Beneficiary Fixture Data ----------------------------------------------------

-- | Fixed beneficiary public key hash
beneficiaryKeyHash :: PubKeyHash
beneficiaryKeyHash = PubKeyHash beneficiaryKeyHashBytes

-- | Fixed beneficiary public key hash as hex-decoded bytes
beneficiaryKeyHashBytes :: BuiltinByteString
beneficiaryKeyHashBytes =
  stringToBuiltinByteStringHex
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

--------------------------------------------------------------------------------
-- Vesting Asset ---------------------------------------------------------------

-- | Fixed currency symbol for the vesting token
vestingCurrencySymbol :: CurrencySymbol
vestingCurrencySymbol =
  CurrencySymbol $
    stringToBuiltinByteStringHex
      "dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

-- | Fixed token name for the vesting token ("vest" in hex)
vestingTokenName :: TokenName
vestingTokenName =
  TokenName $
    stringToBuiltinByteStringHex "76657374"

--------------------------------------------------------------------------------
-- Script Address ---------------------------------------------------------------

-- | Standard script address for UPLC validators
scriptAddr :: Address
scriptAddr =
  Address
    ( ScriptCredential
        (ScriptHash "1111111111111111111111111111111111111111111111111111111111")
    )
    Nothing
