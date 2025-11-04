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
  refundTime,

  -- * Buyer Fixture Data
  buyerKeyHash,
  buyerKeyHashBytes,

  -- * Seller Fixture Data
  sellerKeyHash,
  sellerKeyHashBytes,

  -- * Script Address
  scriptAddr,

  -- * Datum Types for State Management
  EscrowState (..),
  EscrowDatum (..),
  initialEscrowDatum,
) where

import PlutusLedgerApi.Data.V3
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Prelude

--------------------------------------------------------------------------------
-- Escrow Parameters -----------------------------------------------------------

-- | Fixed escrow price in lovelace (75 ADA)
escrowPrice :: Lovelace
escrowPrice = Lovelace 75000000

-- | Escrow deadline in seconds (30 minutes)
escrowDeadlineSeconds :: Integer
escrowDeadlineSeconds = 1800

-- | Refund time in POSIXTime (based on deadline)
refundTime :: POSIXTime
refundTime = POSIXTime escrowDeadlineSeconds

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

--------------------------------------------------------------------------------
-- Datum Types for State Management --------------------------------------------

-- | Escrow state transitions for proper state machine validation
data EscrowState
  = -- | Initial state after buyer deposits funds
    Deposited
  | -- | Seller has accepted payment (final state)
    Accepted
  | -- | Buyer has reclaimed funds (final state)
    Refunded

-- | Complete escrow datum containing state and timing information
data EscrowDatum = EscrowDatum
  { escrowState :: EscrowState
  -- ^ Current state of the escrow
  , depositTime :: POSIXTime
  -- ^ When the deposit was made (for deadline calculations)
  }

-- | Initial datum state when escrow is first created
initialEscrowDatum :: POSIXTime -> EscrowDatum
initialEscrowDatum depositTime =
  EscrowDatum {escrowState = Deposited, depositTime = depositTime}

-- PlutusTx instances for serialization
makeIsDataIndexed
  ''EscrowState
  [('Deposited, 0), ('Accepted, 1), ('Refunded, 2)]
makeIsDataIndexed ''EscrowDatum [('EscrowDatum, 0)]
