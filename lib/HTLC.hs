{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
  HTLCDatum,
  pattern HTLCDatum,
  payer,
  recipient,
  secretHash,
  timeout,
  HTLCRedeemer,
  pattern Claim,
  pattern Refund,
) where

import PlutusLedgerApi.Data.V3
import PlutusTx.AsData (asData)
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

-- The datum and redeemer types are encoded as 'BuiltinData' via 'asData' rather
-- than ordinary algebraic datatypes. The validator only inspects 3 of 4 datum
-- fields per execution path, so lazy field extraction via the generated pattern
-- synonyms is materially cheaper than the eager 'unsafeFromBuiltinData' decode
-- that 'makeIsDataIndexed' would otherwise produce. See
-- https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/optimizing-scripts-with-asData
asData
  [d|
    data HTLCDatum = HTLCDatum
      { payer :: Address
      , recipient :: Address
      , secretHash :: BuiltinByteString
      , timeout :: POSIXTime
      }
      deriving newtype (FromData, ToData, UnsafeFromData)

    data HTLCRedeemer
      = Claim BuiltinByteString
      | Refund
      deriving newtype (FromData, ToData, UnsafeFromData)
    |]

--------------------------------------------------------------------------------
-- Validator -------------------------------------------------------------------

{- | HTLC Validator

Redeemer constants:
  - Claim preimage = 0(preimage) (recipient withdraws by revealing preimage)
  - Refund         = 1()         (payer reclaims after timeout)

Both the datum/redeemer (declared above) and the surrounding ledger types
('ScriptContext', 'TxInfo', 'TxOut', 'Address', …) are 'asData' newtypes. To
avoid re-decoding the underlying 'Data' on each field access, the validator
pattern-matches every layer exactly once and threads the extracted fields
through the per-redeemer branches.
-}
{-# INLINEABLE htlcValidator #-}
htlcValidator :: BuiltinData -> BuiltinUnit
htlcValidator scriptContextData =
  case unsafeFromBuiltinData scriptContextData of
    ScriptContext
      { scriptContextTxInfo =
        TxInfo
          { txInfoInputs
          , txInfoValidRange
          , txInfoSignatories
          }
      , scriptContextRedeemer = Redeemer redeemerBd
      , scriptContextScriptInfo =
        SpendingScript ownTxOutRef (Just (Datum datumBd))
      } ->
        case unsafeFromBuiltinData redeemerBd of
          Claim preimage ->
            case unsafeFromBuiltinData datumBd of
              HTLCDatum {recipient, secretHash, timeout} ->
                validateClaim
                  recipient
                  secretHash
                  timeout
                  preimage
                  txInfoInputs
                  txInfoValidRange
                  txInfoSignatories
                  ownTxOutRef
          Refund ->
            case unsafeFromBuiltinData datumBd of
              HTLCDatum {payer, timeout} ->
                validateRefund
                  payer
                  timeout
                  txInfoInputs
                  txInfoValidRange
                  txInfoSignatories
                  ownTxOutRef
    _ -> traceError "Expected SpendingScript with inline datum"

--------------------------------------------------------------------------------
-- Validation Functions --------------------------------------------------------

-- | Validates claim: recipient reveals preimage before timeout.
{-# INLINEABLE validateClaim #-}
validateClaim ::
  Address ->
  BuiltinByteString ->
  POSIXTime ->
  BuiltinByteString ->
  List.List TxInInfo ->
  POSIXTimeRange ->
  List.List PubKeyHash ->
  TxOutRef ->
  BuiltinUnit
validateClaim
  recipient
  secretHash
  timeout
  preimage
  inputs
  validRange
  signatories
  ownTxOutRef =
    if
      | not (signedBy (extractPubKeyHash recipient) signatories) ->
          traceError "Missing recipient signature"
      | not (equalsByteString (sha2_256 preimage) secretHash) ->
          traceError "Preimage does not match stored hash"
      | lessThanEqualsInteger timeoutInt upperTime ->
          traceError "Claim not permitted at or after timeout"
      | not (equalsInteger (countOwnScriptInputs inputs ownTxOutRef) 1) ->
          traceError "Double satisfaction"
      | otherwise -> unitval
    where
      POSIXTime upperTime = upperBoundTime validRange
      POSIXTime timeoutInt = timeout

-- | Validates refund: payer reclaims funds after timeout.
{-# INLINEABLE validateRefund #-}
validateRefund ::
  Address ->
  POSIXTime ->
  List.List TxInInfo ->
  POSIXTimeRange ->
  List.List PubKeyHash ->
  TxOutRef ->
  BuiltinUnit
validateRefund payer timeout inputs validRange signatories ownTxOutRef =
  if
    | not (signedBy (extractPubKeyHash payer) signatories) ->
        traceError "Missing payer signature"
    | lessThanEqualsInteger currentTime timeoutInt ->
        traceError "Refund not permitted until after timeout"
    | not (equalsInteger (countOwnScriptInputs inputs ownTxOutRef) 1) ->
        traceError "Double satisfaction"
    | otherwise -> unitval
  where
    POSIXTime currentTime = lowerBoundTime validRange
    POSIXTime timeoutInt = timeout

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
lowerBoundTime _ = traceError "Lower bound of valid range must be finite"

{- | Extract the normalised inclusive upper bound from a POSIXTimeRange,
failing if it is not finite. Used by 'validateClaim' to check that the
transaction's validity window ends strictly before the timeout.
-}
{-# INLINEABLE upperBoundTime #-}
upperBoundTime :: POSIXTimeRange -> POSIXTime
upperBoundTime (Interval _ (UpperBound (Finite t) True)) = t
upperBoundTime (Interval _ (UpperBound (Finite (POSIXTime t)) False)) = POSIXTime (t - 1)
upperBoundTime _ = traceError "Upper bound of valid range must be finite"

-- | Extract PubKeyHash from an Address.
{-# INLINEABLE extractPubKeyHash #-}
extractPubKeyHash :: Address -> PubKeyHash
extractPubKeyHash (Address (PubKeyCredential pkh) _) = pkh
extractPubKeyHash _ = traceError "Expected PubKeyCredential address"

{- | Was the transaction signed by the given key? Inlined from
'PlutusLedgerApi.V3.Data.Contexts.txSignedBy' so we operate on the
already-extracted signatories list instead of re-extracting it from TxInfo.
-}
{-# INLINEABLE signedBy #-}
signedBy :: PubKeyHash -> List.List PubKeyHash -> Bool
signedBy = List.elem

{- | Look up the script hash of the input identified by 'ownTxOutRef'. Fails
with a trace error if no such input exists, or if its address is not a
script credential.
-}
{-# INLINEABLE ownInputScriptHash #-}
ownInputScriptHash :: List.List TxInInfo -> TxOutRef -> BuiltinByteString
ownInputScriptHash inputs ownTxOutRef =
  case List.find isOwn inputs of
    Just
      ( TxInInfo
          { txInInfoResolved =
            TxOut {txOutAddress = Address (ScriptCredential (ScriptHash sh)) _}
          }
        ) ->
      sh
    Just _ -> traceError "Own input address is not a script credential"
    Nothing -> traceError "Own input not found"
  where
    isOwn (TxInInfo {txInInfoOutRef = TxOutRef (TxId t) i}) =
      let TxOutRef (TxId t') i' = ownTxOutRef
       in equalsByteString t t' && equalsInteger i i'

{- | Count how many of @inputs@ are spending from a script address with the
same script hash as the input identified by @ownTxOutRef@ (the hash is
resolved via 'ownInputScriptHash'). Operates on the already-extracted
inputs list — no 'TxInfo' accessor calls.
-}
{-# INLINEABLE countOwnScriptInputs #-}
countOwnScriptInputs :: List.List TxInInfo -> TxOutRef -> Integer
countOwnScriptInputs inputs ownTxOutRef =
  let ownHash = ownInputScriptHash inputs ownTxOutRef
   in List.foldl
        ( \acc ( TxInInfo
                  { txInInfoResolved = TxOut {txOutAddress = Address {addressCredential = cred}}
                  }
                ) ->
            case cred of
              ScriptCredential (ScriptHash sh) ->
                if equalsByteString sh ownHash then acc + 1 else acc
              _ -> acc
        )
        0
        inputs
