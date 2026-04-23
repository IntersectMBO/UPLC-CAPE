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

-- | Test fixture data for HTLC benchmark
module HTLC.Fixture (
  -- * Re-exported types
  module HTLC,

  -- * Participants
  payerKeyHash,
  payerKeyHashBytes,
  recipientKeyHash,
  recipientKeyHashBytes,

  -- * HTLC Secret
  correctPreimage,
  wrongPreimage,
  secretHashBytes,
  timeoutPosix,

  -- * Script Address
  scriptAddr,
  scriptHash,
) where

import HTLC (HTLCDatum (..), HTLCRedeemer (..))
import PlutusLedgerApi.Data.V3
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import Prelude

--------------------------------------------------------------------------------
-- Participants ----------------------------------------------------------------

payerKeyHash :: PubKeyHash
payerKeyHash = PubKeyHash payerKeyHashBytes

payerKeyHashBytes :: BuiltinByteString
payerKeyHashBytes =
  stringToBuiltinByteStringHex
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

recipientKeyHash :: PubKeyHash
recipientKeyHash = PubKeyHash recipientKeyHashBytes

recipientKeyHashBytes :: BuiltinByteString
recipientKeyHashBytes =
  stringToBuiltinByteStringHex
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

--------------------------------------------------------------------------------
-- HTLC Secret -----------------------------------------------------------------

-- | Correct preimage (#deadbeef) whose SHA-256 matches 'secretHashBytes'.
correctPreimage :: BuiltinByteString
correctPreimage = stringToBuiltinByteStringHex "deadbeef"

-- | Wrong preimage (#cafebabe) used for negative tests.
wrongPreimage :: BuiltinByteString
wrongPreimage = stringToBuiltinByteStringHex "cafebabe"

-- | SHA-256 digest of 'correctPreimage'.
secretHashBytes :: BuiltinByteString
secretHashBytes =
  stringToBuiltinByteStringHex
    "5f78c33274e43fa9de5659265c1d917e25c03722dcb0b8d27db8d5feaa813953"

-- | Fixed timeout (POSIX timestamp).
timeoutPosix :: POSIXTime
timeoutPosix = POSIXTime 100

--------------------------------------------------------------------------------
-- Script Address --------------------------------------------------------------

-- | Address of the HTLC validator script
scriptAddr :: Address
scriptAddr = Address (ScriptCredential scriptHash) Nothing

-- | Script hash for the HTLC validator
scriptHash :: ScriptHash
scriptHash = ScriptHash "1111111111111111111111111111111111111111111111111111111111"
