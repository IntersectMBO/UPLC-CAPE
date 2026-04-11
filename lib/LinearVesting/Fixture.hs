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
  -- * Re-exported types
  module LinearVesting,

  -- * Beneficiary Fixture Data
  beneficiaryKeyHash,
  beneficiaryKeyHashBytes,

  -- * Vesting Asset
  vestingCurrencySymbol,
  vestingTokenName,

  -- * Script Address
  scriptAddr,
) where

import LinearVesting (VestingDatum (..), VestingRedeemer (..))
import PlutusLedgerApi.Data.V3
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Prelude

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
