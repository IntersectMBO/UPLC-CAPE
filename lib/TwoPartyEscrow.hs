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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module TwoPartyEscrow (twoPartyEscrowValidatorCode) where

import PlutusLedgerApi.Data.V3
import PlutusTx
import PlutusTx.Prelude

import PlutusLedgerApi.V1.Data.Value (lovelaceValueOf)
import PlutusLedgerApi.V3.Data.Contexts (
  getContinuingOutputs,
  txSignedBy,
  valuePaidTo,
 )
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.Data.List qualified as List
import TwoPartyEscrow.Fixture qualified as Fixed

{- | Redeemer constants for documentation
Deposit = 0, Accept = 1, Refund = 2
-}

{- | Two-Party Escrow Validator
Takes BuiltinData-encoded ScriptContext and returns BuiltinUnit
-}
{-# INLINEABLE twoPartyEscrowValidator #-}
twoPartyEscrowValidator :: BuiltinData -> BuiltinUnit
twoPartyEscrowValidator scriptContextData =
  case red of
    (0 :: Integer) ->
      let outs = getContinuingOutputs ctx
          outCount = List.length outs
       in if
            | outCount == 0 ->
                traceError "No continuing outputs found"
            | outCount > 1 ->
                traceError "Too many continuing outputs found"
            | not (txSignedBy (scriptContextTxInfo ctx) Fixed.buyerKeyHash) ->
                traceError "Buyer signature missing"
            | let onlyOut = List.head outs
               in lovelaceValueOf (txOutValue onlyOut) /= Fixed.escrowPrice ->
                traceError "Wrong continuing output amount"
            | otherwise -> unitval
    1 ->
      -- Accept: seller accepts payment (75 ADA should go to seller)
      if
        | not (txSignedBy (scriptContextTxInfo ctx) Fixed.sellerKeyHash) ->
            traceError "Seller signature missing"
        | lovelaceValueOf (valuePaidTo (scriptContextTxInfo ctx) Fixed.sellerKeyHash)
            /= Fixed.escrowPrice ->
            traceError "Incorrect payment to seller"
        | otherwise -> unitval
    2 -> traceError "Refund not implemented"
    _ -> traceError "Invalid redeemer"
  where
    ctx = unsafeFromBuiltinData scriptContextData
    red = unsafeFromBuiltinData (getRedeemer (scriptContextRedeemer ctx))

-- | Compiled validator code
twoPartyEscrowValidatorCode :: CompiledCode (BuiltinData -> BuiltinUnit)
twoPartyEscrowValidatorCode = $$(PlutusTx.compile [||twoPartyEscrowValidator||])
