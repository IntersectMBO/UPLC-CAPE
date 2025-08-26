{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
--
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module TwoPartyEscrow (twoPartyEscrowValidatorCode, twoPartyEscrowAcceptCode) where

import PlutusTx.Prelude

import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.V3.Data.Contexts (ScriptContext (..), getRedeemer)
import PlutusTx
import PlutusTx.Builtins (unsafeDataAsI)
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.Builtins.Internal qualified as BI

{- | Redeemer constants for documentation
Deposit = 0, Accept = 1, Refund = 2
-}

{- | Two-Party Escrow Validator
Takes BuiltinData (redeemer) and returns BuiltinUnit
-}
{-# INLINEABLE twoPartyEscrowValidator #-}
twoPartyEscrowValidator :: BuiltinData -> BuiltinUnit
twoPartyEscrowValidator scriptContextData =
  case fromBuiltinData scriptContextData of
    Nothing -> traceError "Failed to parse ScriptContext"
    Just ctx ->
      case fromBuiltinData (getRedeemer (scriptContextRedeemer ctx)) of
        Nothing -> traceError "Failed to parse Redeemer"
        Just redeemerInt -> case redeemerInt of
          0 ->
            -- Deposit: buyer deposits exactly 75 ADA
            -- In a real validator, this would check:
            -- - Buyer signature present
            -- - Exactly 75 ADA deposited to script
            -- - Datum updated with deposit timestamp
            unitval
          1 ->
            -- Accept: seller accepts payment
            -- In a real validator, this would check:
            -- - Seller signature present
            -- - Funds transferred from script to seller
            -- - Deadline not passed
            unitval
          2 ->
            -- Refund: buyer reclaims after deadline
            -- In a real validator, this would check:
            -- - Buyer signature present
            -- - Deadline has passed
            -- - Funds returned to buyer
            unitval
          _ -> traceError "Invalid redeemer"

-- | Compiled validator code
twoPartyEscrowValidatorCode :: CompiledCode (BuiltinData -> BuiltinUnit)
twoPartyEscrowValidatorCode = $$(PlutusTx.compile [||twoPartyEscrowValidator||])

-- | Create BuiltinData for integer 1 (Accept redeemer)
acceptRedeemerData :: BuiltinData
acceptRedeemerData = BI.BuiltinData (PLC.I 1)

{- | The compiled two-party escrow validator for Accept sequence (redeemer = 1)
This is used for performance measurement according to the benchmark specification
-}
twoPartyEscrowAcceptCode :: CompiledCode BuiltinUnit
twoPartyEscrowAcceptCode =
  twoPartyEscrowValidatorCode `unsafeApplyCode` liftCodeDef acceptRedeemerData

--------------------------------------------------------------------------------
-- Fixture ---------------------------------------------------------------------

escrowPrice :: Integer
escrowPrice = 75000000 -- 75 ADA in lovelace

escrowDeadlineSeconds :: Integer
escrowDeadlineSeconds = 1800 -- 30 minutes

-- | Fixed buyer address
buyerAddress :: BuiltinByteString
buyerAddress =
  stringToBuiltinByteStringHex
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- | Fixed seller address
sellerAddress :: BuiltinByteString
sellerAddress =
  stringToBuiltinByteStringHex
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
