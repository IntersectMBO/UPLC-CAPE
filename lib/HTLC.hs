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

module HTLC (
  htlcValidator,
  HTLCDatum (..),
  HTLCRedeemer (..),
) where

import PlutusLedgerApi.Data.V3
import PlutusLedgerApi.V3.Data.Contexts (findOwnInput, txSignedBy)
import PlutusTx
import PlutusTx.Builtins (
  equalsByteString,
  equalsInteger,
  lessThanEqualsInteger,
 )
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.Data.List qualified as List
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Datum and Redeemer Types ----------------------------------------------------

-- | HTLC parameters stored on-chain as inline datum
data HTLCDatum = HTLCDatum
  { payer :: Address
  , recipient :: Address
  , secretHash :: BuiltinByteString
  , timeout :: POSIXTime
  }

-- | Redeemer actions for the HTLC validator
data HTLCRedeemer
  = Claim BuiltinByteString
  | Refund

makeIsDataIndexed ''HTLCDatum [('HTLCDatum, 0)]
makeIsDataIndexed ''HTLCRedeemer [('Claim, 0), ('Refund, 1)]

--------------------------------------------------------------------------------
-- Validator -------------------------------------------------------------------

{- | HTLC Validator

Redeemer constants:
  - Claim preimage = 0(preimage) (recipient withdraws by revealing preimage)
  - Refund         = 1()         (payer reclaims after timeout)

The validator reads HTLCDatum from the ScriptInfo datum.
-}
{-# INLINEABLE htlcValidator #-}
htlcValidator :: BuiltinData -> BuiltinUnit
htlcValidator scriptContextData =
  case redeemer of
    Claim preimage -> validateClaim ctx datum preimage
    Refund -> validateRefund ctx datum
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData scriptContextData

    redeemer :: HTLCRedeemer
    redeemer = unsafeFromBuiltinData (getRedeemer (scriptContextRedeemer ctx))

    datum :: HTLCDatum
    datum = spendingDatum (scriptContextScriptInfo ctx)

--------------------------------------------------------------------------------
-- Validation Functions --------------------------------------------------------

-- | Validates claim: recipient reveals preimage before timeout.
{-# INLINEABLE validateClaim #-}
validateClaim :: ScriptContext -> HTLCDatum -> BuiltinByteString -> BuiltinUnit
validateClaim ctx HTLCDatum {recipient, secretHash, timeout} preimage =
  if
    | not signed ->
        traceError "Missing recipient signature"
    | not (equalsByteString (sha2_256 preimage) secretHash) ->
        traceError "Preimage does not match stored hash"
    | lessThanEqualsInteger timeoutInt currentTime ->
        traceError "Claim not permitted at or after timeout"
    | not (equalsInteger (countScriptInputs txInfo ownScriptHash) 1) ->
        traceError "Double satisfaction"
    | otherwise -> unitval
  where
    txInfo = scriptContextTxInfo ctx
    recipientHash = extractPubKeyHash recipient
    signed = txSignedBy txInfo (PubKeyHash recipientHash)
    POSIXTime currentTime = lowerBoundTime (txInfoValidRange txInfo)
    POSIXTime timeoutInt = timeout
    ownScriptHash = ownInputScriptHash ctx

-- | Validates refund: payer reclaims funds after timeout.
{-# INLINEABLE validateRefund #-}
validateRefund :: ScriptContext -> HTLCDatum -> BuiltinUnit
validateRefund ctx HTLCDatum {payer, timeout} =
  if
    | not (txSignedBy txInfo (PubKeyHash payerHash)) ->
        traceError "Missing payer signature"
    | lessThanEqualsInteger currentTime timeoutInt ->
        traceError "Refund not permitted until after timeout"
    | not (equalsInteger (countScriptInputs txInfo ownScriptHash) 1) ->
        traceError "Double satisfaction"
    | otherwise -> unitval
  where
    txInfo = scriptContextTxInfo ctx
    payerHash = extractPubKeyHash payer
    POSIXTime currentTime = lowerBoundTime (txInfoValidRange txInfo)
    POSIXTime timeoutInt = timeout
    ownScriptHash = ownInputScriptHash ctx

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

{- | Extract the normalised inclusive lower bound from a POSIXTimeRange,
failing if it is not finite. Mirrors the behaviour of
'PlutusLedgerApi.V1.Data.Interval.inclusiveLowerBound', which is defined
there but not re-exported.
-}
{-# INLINEABLE lowerBoundTime #-}
lowerBoundTime :: POSIXTimeRange -> POSIXTime
lowerBoundTime (Interval (LowerBound (Finite t) True) _) = t
lowerBoundTime (Interval (LowerBound (Finite (POSIXTime t)) False) _) = POSIXTime (t + 1)
lowerBoundTime _ = traceError "Time range not Finite"

-- | Extract PubKeyHash bytes from an Address.
{-# INLINEABLE extractPubKeyHash #-}
extractPubKeyHash :: Address -> BuiltinByteString
extractPubKeyHash (Address (PubKeyCredential (PubKeyHash pkh)) _) = pkh
extractPubKeyHash _ = traceError "Expected PubKeyCredential address"

-- | Count inputs whose address is a script credential matching the given hash.
{-# INLINEABLE countScriptInputs #-}
countScriptInputs :: TxInfo -> BuiltinByteString -> Integer
countScriptInputs txInfo scriptHash =
  List.foldl
    ( \acc TxInInfo {txInInfoResolved = TxOut {txOutAddress}} ->
        case txOutAddress of
          Address (ScriptCredential (ScriptHash sh)) _ ->
            if sh == scriptHash then acc + 1 else acc
          _ -> acc
    )
    0
    (txInfoInputs txInfo)

-- | Look up the script hash of the currently-spending input.
{-# INLINEABLE ownInputScriptHash #-}
ownInputScriptHash :: ScriptContext -> BuiltinByteString
ownInputScriptHash ctx = case findOwnInput ctx of
  Just TxInInfo {txInInfoResolved = TxOut {txOutAddress}} ->
    case txOutAddress of
      Address (ScriptCredential (ScriptHash sh)) _ -> sh
      _ -> traceError "Own input address is not a script credential"
  Nothing -> traceError "Own input not found"

-- | Extract HTLCDatum from SpendingScript info.
{-# INLINEABLE spendingDatum #-}
spendingDatum :: ScriptInfo -> HTLCDatum
spendingDatum = \case
  SpendingScript _ (Just datum) -> unsafeFromBuiltinData (getDatum datum)
  _ -> traceError "Expected SpendingScript with datum"

-- NOTE: CompiledCode splice moved to plinth-submissions-app/Main.hs
-- as a workaround for PlutusTx plugin bug: having $$(compile ...) here
-- (without BuiltinCasing) prevents cross-library re-compilation with
-- BuiltinCasing in Preview.HTLC.
