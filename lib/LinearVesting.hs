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

module LinearVesting (
  linearVestingValidatorCode,
  linearVestingValidator,
  VestingDatum (..),
  VestingRedeemer (..),
) where

import PlutusLedgerApi.Data.V3
import PlutusTx
import PlutusTx.Prelude
import PlutusLedgerApi.V1.Data.Value (valueOf)
import PlutusLedgerApi.V3.Data.Contexts (
  findOwnInput,
  getContinuingOutputs,
  txSignedBy,
 )
import PlutusTx.Builtins (equalsInteger, lessThanEqualsInteger)
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.Data.List qualified as List

--------------------------------------------------------------------------------
-- Datum and Redeemer Types ----------------------------------------------------

-- | Vesting parameters stored on-chain as inline datum
data VestingDatum = VestingDatum
  { beneficiary :: Address
  , vestingAsset :: (CurrencySymbol, TokenName)
  , totalVestingQty :: Integer
  , vestingPeriodStart :: Integer
  , vestingPeriodEnd :: Integer
  , firstUnlockPossibleAfter :: Integer
  , totalInstallments :: Integer
  }

-- | Redeemer actions for the vesting validator
data VestingRedeemer
  = PartialUnlock
  | FullUnlock

makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]
makeIsDataIndexed ''VestingRedeemer [('PartialUnlock, 0), ('FullUnlock, 1)]

--------------------------------------------------------------------------------
-- Validator -------------------------------------------------------------------

{- | Linear Vesting Validator

Redeemer constants:
  - PartialUnlock = 0() (withdraw proportional tokens)
  - FullUnlock    = 1() (withdraw all after period ends)

The validator reads VestingDatum from the ScriptInfo datum, not baked-in constants.
All vesting parameters (beneficiary, asset, schedule) come from the datum.
-}
{-# INLINEABLE linearVestingValidator #-}
linearVestingValidator :: BuiltinData -> BuiltinUnit
linearVestingValidator scriptContextData =
  case redeemer of
    PartialUnlock -> validatePartialUnlock ctx datum
    FullUnlock -> validateFullUnlock ctx datum
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData scriptContextData

    redeemer :: VestingRedeemer
    redeemer = unsafeFromBuiltinData (getRedeemer (scriptContextRedeemer ctx))

    datum :: VestingDatum
    datum = spendingDatum (scriptContextScriptInfo ctx)

--------------------------------------------------------------------------------
-- Validation Functions --------------------------------------------------------

-- | Validates partial unlock: proportional withdrawal during vesting period.
{-# INLINEABLE validatePartialUnlock #-}
validatePartialUnlock :: ScriptContext -> VestingDatum -> BuiltinUnit
validatePartialUnlock ctx VestingDatum {beneficiary, vestingAsset, totalVestingQty, vestingPeriodStart, vestingPeriodEnd, firstUnlockPossibleAfter, totalInstallments} =
  if
    | not signed ->
        traceError "Missing beneficiary signature"
    | lessThanEqualsInteger currentTime firstUnlockPossibleAfter ->
        traceError "Unlock not permitted until firstUnlockPossibleAfter time"
    | lessThanEqualsInteger newRemainingQty 0 ->
        traceError "Zero remaining assets not allowed"
    | lessThanEqualsInteger oldRemainingQty newRemainingQty ->
        traceError "Remaining asset is not decreasing"
    | not (equalsInteger expectedRemainingQty newRemainingQty) ->
        traceError "Mismatched remaining asset"
    | inputDatum /= outputDatum ->
        traceError "Datum Modification Prohibited"
    | not (equalsInteger (countScriptInputs txInfo scriptHash) 1) ->
        traceError "Double satisfaction"
    | otherwise -> unitval
  where
    txInfo = scriptContextTxInfo ctx

    -- Beneficiary signature check
    beneficiaryHash = extractPubKeyHash beneficiary
    signed = txSignedBy txInfo (PubKeyHash beneficiaryHash)

    -- Time extraction from valid range lower bound
    currentTime = lowerBoundTime (txInfoValidRange txInfo)

    -- Find own input and continuing output
    !ownInput = case findOwnInput ctx of
      Just txInInfo -> txInInfoResolved txInInfo
      Nothing -> traceError "Own input not found"

    continuingOuts = getContinuingOutputs ctx
    !continuingOut = case List.uncons continuingOuts of
      Just (out, _) -> out
      Nothing -> traceError "Own output not found"

    -- Asset quantities
    (cs, tn) = vestingAsset
    oldRemainingQty = valueOf (txOutValue ownInput) cs tn
    newRemainingQty = valueOf (txOutValue continuingOut) cs tn

    -- Vesting schedule calculation
    vestingPeriodLength = vestingPeriodEnd - vestingPeriodStart
    vestingTimeRemaining = vestingPeriodEnd - currentTime
    timeBetweenTwoInstallments = divCeil vestingPeriodLength totalInstallments
    futureInstallments = divCeil vestingTimeRemaining timeBetweenTwoInstallments
    expectedRemainingQty =
      divCeil (futureInstallments * totalVestingQty) totalInstallments

    -- Datum preservation check
    inputDatum = txOutDatum ownInput
    outputDatum = txOutDatum continuingOut

    -- Script hash for double satisfaction check
    scriptHash = extractScriptHash ownInput

-- | Validates full unlock: complete withdrawal after vesting period ends.
{-# INLINEABLE validateFullUnlock #-}
validateFullUnlock :: ScriptContext -> VestingDatum -> BuiltinUnit
validateFullUnlock ctx VestingDatum {beneficiary, vestingPeriodEnd} =
  if
    | not (txSignedBy txInfo (PubKeyHash beneficiaryHash)) ->
        traceError "Missing beneficiary signature"
    | lessThanEqualsInteger currentTime vestingPeriodEnd ->
        traceError "Unlock not permitted until vestingPeriodEnd time"
    | otherwise -> unitval
  where
    txInfo = scriptContextTxInfo ctx
    beneficiaryHash = extractPubKeyHash beneficiary
    currentTime = lowerBoundTime (txInfoValidRange txInfo)

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Integer ceiling division: divCeil(x, y) = 1 + ((x - 1) / y)
{-# INLINEABLE divCeil #-}
divCeil :: Integer -> Integer -> Integer
divCeil x y = 1 + divide (x - 1) y

-- | Extract lower inclusive time from a POSIXTimeRange.
{-# INLINEABLE lowerBoundTime #-}
lowerBoundTime :: POSIXTimeRange -> Integer
lowerBoundTime (Interval (LowerBound (Finite (POSIXTime t)) True) _) = t
lowerBoundTime (Interval (LowerBound (Finite (POSIXTime t)) False) _) = t + 1
lowerBoundTime _ = traceError "Time range not Finite"

-- | Extract PubKeyHash bytes from an Address.
{-# INLINEABLE extractPubKeyHash #-}
extractPubKeyHash :: Address -> BuiltinByteString
extractPubKeyHash (Address (PubKeyCredential (PubKeyHash pkh)) _) = pkh
extractPubKeyHash _ = traceError "Expected PubKeyCredential address"

-- | Extract script hash from a TxOut address.
{-# INLINEABLE extractScriptHash #-}
extractScriptHash :: TxOut -> BuiltinByteString
extractScriptHash TxOut {txOutAddress = Address (ScriptCredential (ScriptHash sh)) _} = sh
extractScriptHash _ = traceError "Expected ScriptCredential address"

-- | Count inputs from a specific script address.
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

-- | Extract VestingDatum from SpendingScript info.
{-# INLINEABLE spendingDatum #-}
spendingDatum :: ScriptInfo -> VestingDatum
spendingDatum = \case
  SpendingScript _ (Just datum) -> unsafeFromBuiltinData (getDatum datum)
  _ -> traceError "Expected SpendingScript with datum"

-- | Compiled validator code
linearVestingValidatorCode :: CompiledCode (BuiltinData -> BuiltinUnit)
linearVestingValidatorCode = $$(PlutusTx.compile [||linearVestingValidator||])
