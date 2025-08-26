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

-- | Test fixture data for TwoPartyEscrow benchmark
module TwoPartyEscrow.Fixture (
  -- * Escrow Parameters
  escrowPrice,
  escrowDeadlineSeconds,

  -- * Buyer Fixture Data
  buyerKeyHash,
  buyerKeyHashBytes,

  -- * Seller Fixture Data
  sellerKeyHash,
  sellerKeyHashBytes,
) where

import PlutusLedgerApi.V1.Data.Value (Lovelace (..))
import PlutusLedgerApi.V3 (BuiltinByteString, PubKeyHash (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Prelude (Integer)

--------------------------------------------------------------------------------
-- Escrow Parameters -----------------------------------------------------------

-- | Fixed escrow price in lovelace (75 ADA)
escrowPrice :: Lovelace
escrowPrice = Lovelace 75000000

-- | Escrow deadline in seconds (30 minutes)
escrowDeadlineSeconds :: Integer
escrowDeadlineSeconds = 1800

--------------------------------------------------------------------------------
-- Buyer Fixture Data ----------------------------------------------------------

-- | Fixed buyer public key hash
buyerKeyHash :: PubKeyHash
buyerKeyHash = PubKeyHash buyerKeyHashBytes

-- | Fixed buyer public key hash as hex-decoded bytes
buyerKeyHashBytes :: BuiltinByteString
buyerKeyHashBytes =
  stringToBuiltinByteStringHex
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

--------------------------------------------------------------------------------
-- Seller Fixture Data ---------------------------------------------------------

-- | Fixed seller public key hash
sellerKeyHash :: PubKeyHash
sellerKeyHash = PubKeyHash sellerKeyHashBytes

-- | Fixed seller public key hash as hex-decoded bytes
sellerKeyHashBytes :: BuiltinByteString
sellerKeyHashBytes =
  stringToBuiltinByteStringHex
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
