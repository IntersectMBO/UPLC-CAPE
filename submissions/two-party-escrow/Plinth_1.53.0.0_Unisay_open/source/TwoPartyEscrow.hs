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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module TwoPartyEscrow (twoPartyEscrowValidatorCode) where

import PlutusLedgerApi.Data.V3
import PlutusTx
import PlutusTx.Prelude

import PlutusLedgerApi.V1.Data.Interval
import PlutusLedgerApi.V1.Data.Value (lovelaceValueOf)
import PlutusLedgerApi.V3.Data.Contexts (
  getContinuingOutputs,
  txSignedBy,
  valuePaidTo,
 )
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.Data.List (List)
import PlutusTx.Data.List qualified as List
import TwoPartyEscrow.Fixture (EscrowDatum (..), EscrowState (..))
import TwoPartyEscrow.Fixture qualified as Fixed

{- | Two-Party Escrow Validator with Datum-Based State Management

Redeemer constants for documentation:
  - Deposit = 0
  - Accept  = 1
  - Refund  = 2

State transitions:
  - Deposit: Initial → Deposited (creates escrow with Deposited state)
  - Accept:  Deposited → Accepted (seller accepts, funds go to seller)
  - Refund:  Deposited → Refunded (buyer reclaims after deadline)

Invalid transitions are rejected to prevent double-spending and state violations.
-}
{-# INLINEABLE twoPartyEscrowValidator #-}
twoPartyEscrowValidator :: BuiltinData -> BuiltinUnit
twoPartyEscrowValidator scriptContextData =
  case redeemer of
    0 -> validateDeposit ctx
    1 -> validateAccept ctx
    2 -> validateRefund ctx
    (_ :: Integer) -> traceError "Invalid redeemer"
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData scriptContextData

    redeemer :: Integer
    redeemer = unsafeFromBuiltinData (getRedeemer (scriptContextRedeemer ctx))

--------------------------------------------------------------------------------
-- Validation Functions --------------------------------------------------------

-- | Validates buyer deposit operation, creating escrow UTXO with Deposited state.
{-# INLINEABLE validateDeposit #-}
validateDeposit :: ScriptContext -> BuiltinUnit
validateDeposit ctx =
  if
    | outCount == 0 ->
        traceError "No script outputs created"
    | outCount > 1 ->
        traceError "Too many script outputs created"
    | missingSignature txInfo Fixed.buyerKeyHash ->
        traceError "Buyer signature missing"
    | unexpectedAmountInScriptOutput (List.head scriptOuts) ->
        traceError "Wrong script output amount"
    | invalidDepositDatum (List.head scriptOuts) (txInfoValidRange txInfo) ->
        traceError "Invalid or missing deposit datum"
    | otherwise -> unitval
  where
    txInfo = scriptContextTxInfo ctx
    scriptOuts = getScriptOutputs txInfo
    outCount = List.length scriptOuts

-- | Validates seller accept operation, paying escrow funds to seller.
{-# INLINEABLE validateAccept #-}
validateAccept :: ScriptContext -> BuiltinUnit
validateAccept ctx =
  case currentState of
    Deposited ->
      if
        | outCount > 0 ->
            traceError "Incomplete withdrawal - funds remain in script"
        | missingSignature txInfo Fixed.sellerKeyHash ->
            traceError "Seller signature missing"
        | missingEscrowInput (txInfoInputs txInfo) ->
            traceError "No valid escrow deposit found in inputs"
        | escrowValueNotPaidTo txInfo Fixed.sellerKeyHash ->
            traceError "Incorrect payment to seller"
        | otherwise -> unitval
    _ ->
      traceError "Accept only valid from Deposited state"
  where
    currentState = escrowState (spendingScriptDatum scriptInfo)
    outs = getContinuingOutputs ctx
    outCount = List.length outs
    txInfo = scriptContextTxInfo ctx
    scriptInfo = scriptContextScriptInfo ctx

-- | Validates buyer refund operation, returning escrow funds to buyer after deadline.
{-# INLINEABLE validateRefund #-}
validateRefund :: ScriptContext -> BuiltinUnit
validateRefund ctx =
  case currentState of
    Deposited ->
      if
        | missingSignature txInfo Fixed.buyerKeyHash ->
            traceError "Buyer signature missing"
        | not (from refundDeadline `contains` nowRange) ->
            traceError "Refund time not reached"
        | missingEscrowInput (txInfoInputs txInfo) ->
            traceError "No valid escrow deposit found in inputs"
        | escrowValueNotPaidTo txInfo Fixed.buyerKeyHash ->
            traceError "Incorrect refund to buyer"
        | otherwise -> unitval
    _ ->
      traceError "Refund only valid from Deposited state"
  where
    currentState = escrowState currentDatum
    currentDatum = spendingScriptDatum (scriptContextScriptInfo ctx)
    txInfo = scriptContextTxInfo ctx
    nowRange = txInfoValidRange txInfo
    refundDeadline = succ (Fixed.depositTime currentDatum + Fixed.refundTime)

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Filters transaction outputs to only those sent to the escrow script address.
{-# INLINEABLE getScriptOutputs #-}
getScriptOutputs :: TxInfo -> List TxOut
getScriptOutputs txInfo = List.filter isScriptOutput (txInfoOutputs txInfo)
  where
    isScriptOutput txOut = txOutAddress txOut == Fixed.scriptAddr

-- | Checks if script output contains incorrect escrow amount.
{-# INLINEABLE unexpectedAmountInScriptOutput #-}
unexpectedAmountInScriptOutput :: TxOut -> Bool
unexpectedAmountInScriptOutput onlyOut =
  lovelaceValueOf (txOutValue onlyOut) /= Fixed.escrowPrice

-- | Validates deposit datum has correct state and timestamp for current transaction.
{-# INLINEABLE invalidDepositDatum #-}
invalidDepositDatum :: TxOut -> POSIXTimeRange -> Bool
invalidDepositDatum onlyOut validRange =
  case txOutDatum onlyOut of
    OutputDatum datum ->
      let escrowDatum = unsafeFromBuiltinData (getDatum datum)
          currentTime = case validRange of
            Interval (LowerBound (Finite t) _) _ -> t
            _ -> traceError "No valid time range provided"
       in case escrowState escrowDatum of
            Deposited -> Fixed.depositTime escrowDatum /= currentTime
            _ -> True -- Invalid if not Deposited state
    _ -> True -- Invalid if no datum

-- | Checks if required signature is missing from transaction.
{-# INLINEABLE missingSignature #-}
missingSignature :: TxInfo -> PubKeyHash -> Bool
missingSignature txInfo keyHash = not (txSignedBy txInfo keyHash)

-- | Checks if transaction lacks a valid escrow input with correct amount.
{-# INLINEABLE missingEscrowInput #-}
missingEscrowInput :: List TxInInfo -> Bool
missingEscrowInput =
  List.all \TxInInfo {txInInfoResolved = TxOut {txOutAddress, txOutValue}} ->
    case txOutAddress of
      Address (ScriptCredential _) _ ->
        lovelaceValueOf txOutValue /= Fixed.escrowPrice
      _ -> True

-- | Checks if escrow amount was not paid to the specified key hash.
{-# INLINEABLE escrowValueNotPaidTo #-}
escrowValueNotPaidTo :: TxInfo -> PubKeyHash -> Bool
escrowValueNotPaidTo txInfo keyHash =
  lovelaceValueOf (valuePaidTo txInfo keyHash) /= Fixed.escrowPrice

-- | Extracts escrow datum from spending script context.
{-# INLINEABLE spendingScriptDatum #-}
spendingScriptDatum :: ScriptInfo -> EscrowDatum
spendingScriptDatum = \case
  SpendingScript _ (Just datum) -> unsafeFromBuiltinData (getDatum datum)
  _ -> traceError "Expected SpendingScript with datum"

-- | Compiled validator code
twoPartyEscrowValidatorCode :: CompiledCode (BuiltinData -> BuiltinUnit)
twoPartyEscrowValidatorCode = $$(PlutusTx.compile [||twoPartyEscrowValidator||])
