module Cape.ScriptContextBuilder (
  ScriptContextBuilder (..),
  BaselineType (..),
  PatchOperation (..),
  Purpose (..),
  BuildError (..),
  buildScriptContext,
  applyPatches,
  applyPatch,
  createBaseline,
) where

import PlutusLedgerApi.Data.V3
import Prelude

import Control.Monad (foldM)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Json
import Data.Text qualified as Text
import PlutusTx.Data.AssocMap qualified as Map
import PlutusTx.Data.List qualified as List

-- | Baseline template types for ScriptContext generation
data BaselineType = SpendingBaseline
  deriving stock (Show, Eq, Generic)

-- | Individual patch operations that can be applied to a ScriptContext
data PatchOperation
  = AddSignature PubKeyHash
  | RemoveSignature PubKeyHash
  | SetRedeemer Redeemer
  | {- | Add an input. Without an address the input sits at the placeholder
    script address when it is the script's own input, and at an empty
    public key hash otherwise. The last field is its reference script.
    -}
    AddInputUTXO
      TxOutRef
      (Maybe Address)
      Value
      Bool
      OutputDatum
      (Maybe ScriptHash)
  | AddInputUTXORaw TxInInfo
  | SetValidRange (Maybe POSIXTime) (Maybe POSIXTime)
  | SetValidRangeRaw POSIXTimeRange
  | AddOutputUTXO Address Value
  | RemoveOutputUTXO Int
  | SetScriptDatum Datum
  | AddOutputUTXOWithDatum Address Value Datum
  | -- | Add an output exactly as given, reference script included.
    AddOutputUTXORaw TxOut
  | {- | Switch the script purpose to minting under the given policy,
    keeping the transaction as it is.
    -}
    SetMintingScript CurrencySymbol
  | {- | Switch the script purpose to rewarding for the given credential,
    keeping the transaction as it is.
    -}
    SetRewardingScript Credential
  | -- | Replace the whole mint field of the transaction.
    SetMint MintValue
  | -- | Set the transaction fee.
    SetFee Lovelace
  | -- | Set the transaction id.
    SetTxId TxId
  | {- | Replace the redeemers of the transaction, keeping the given order.
    This is the map every script of the transaction sees; it is
    independent of 'SetRedeemer', the redeemer of the script under test.
    -}
    SetRedeemers [(Purpose, Redeemer)]
  deriving stock (Show, Eq, Generic)

-- | The script purposes a redeemer can be recorded for.
data Purpose
  = SpendingPurpose TxOutRef
  | MintingPurpose CurrencySymbol
  | RewardingPurpose Credential
  deriving stock (Show, Eq, Generic)

-- | Builder specification combining baseline and patches
data ScriptContextBuilder = ScriptContextBuilder
  { baselineType :: BaselineType
  , patches :: [PatchOperation]
  }
  deriving stock (Show, Eq, Generic)

-- | Errors that can occur during ScriptContext building
data BuildError
  = InvalidBaseline Text.Text
  | PatchApplicationError Text.Text PatchOperation
  | IncompatiblePatches Text.Text
  deriving stock (Show, Eq)

-- | Build a complete ScriptContext from a builder specification
buildScriptContext :: ScriptContextBuilder -> Either BuildError ScriptContext
buildScriptContext ScriptContextBuilder {baselineType, patches} = do
  let baseline = createBaseline baselineType
  applyPatches patches baseline

-- | Create a baseline ScriptContext template
createBaseline :: BaselineType -> ScriptContext
createBaseline SpendingBaseline =
  ScriptContext
    { scriptContextTxInfo = createMinimalTxInfo
    , scriptContextRedeemer = Redeemer (toBuiltinData ())
    , scriptContextScriptInfo = SpendingScript dummyTxOutRef (Just dummyDatum)
    }
  where
    createMinimalTxInfo :: TxInfo
    createMinimalTxInfo =
      TxInfo
        { txInfoInputs = mempty
        , txInfoReferenceInputs = mempty
        , txInfoOutputs = mempty
        , txInfoFee = Lovelace 0
        , txInfoMint = emptyMintValue
        , txInfoTxCerts = mempty
        , txInfoWdrl = Map.empty
        , txInfoValidRange = always
        , txInfoSignatories = mempty
        , txInfoRedeemers = Map.empty
        , txInfoData = Map.empty
        , txInfoId = dummyTxId
        , txInfoVotes = Map.empty
        , txInfoProposalProcedures = mempty
        , txInfoCurrentTreasuryAmount = Nothing
        , txInfoTreasuryDonation = Nothing
        }

    dummyTxId :: TxId
    dummyTxId = TxId "0000000000000000000000000000000000000000000000000000000000000000"

    dummyTxOutRef :: TxOutRef
    dummyTxOutRef = TxOutRef dummyTxId 0

    dummyDatum :: Datum
    dummyDatum = Datum (toBuiltinData ())

-- | Apply a sequence of patch operations to a ScriptContext
applyPatches ::
  [PatchOperation] -> ScriptContext -> Either BuildError ScriptContext
applyPatches patches scriptContext = foldM applyPatch scriptContext patches

-- | Apply a single patch operation to a ScriptContext
applyPatch ::
  ScriptContext -> PatchOperation -> Either BuildError ScriptContext
applyPatch ctx patch =
  case patch of
    AddSignature pubKeyHash -> do
      let txInfo = scriptContextTxInfo ctx
          updatedTxInfo =
            txInfo
              { txInfoSignatories = List.cons pubKeyHash (txInfoSignatories txInfo)
              }
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    RemoveSignature pubKeyHash -> do
      let txInfo = scriptContextTxInfo ctx
          currentSignatories = txInfoSignatories txInfo
          filteredSignatories = List.filter (/= pubKeyHash) currentSignatories
          updatedTxInfo = txInfo {txInfoSignatories = filteredSignatories}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    SetRedeemer redeemer -> do
      pure $ ctx {scriptContextRedeemer = redeemer}
    AddInputUTXO txOutRef explicitAddr value isOwnInput outputDatum referenceScript -> do
      let txInfo = scriptContextTxInfo ctx
          -- Use script address for script inputs, dummy address for regular inputs
          defaultAddr =
            if isOwnInput
              then
                Address
                  ( ScriptCredential
                      (ScriptHash "1111111111111111111111111111111111111111111111111111111111")
                  )
                  Nothing
              else Address (PubKeyCredential (PubKeyHash "")) Nothing
          inputAddr = fromMaybe defaultAddr explicitAddr
          newTxIn =
            TxInInfo txOutRef (TxOut inputAddr value outputDatum referenceScript)
          updatedInputs = List.cons newTxIn (txInfoInputs txInfo)
          updatedTxInfo = txInfo {txInfoInputs = updatedInputs}
          updatedCtx = ctx {scriptContextTxInfo = updatedTxInfo}
      -- If this is the script's own input, also update the spending script info
      if isOwnInput
        then case scriptContextScriptInfo ctx of
          SpendingScript _ maybeDatum ->
            pure $
              updatedCtx {scriptContextScriptInfo = SpendingScript txOutRef maybeDatum}
          _ ->
            Left $
              PatchApplicationError
                "AddInputUTXO with is_own_input=true can only be applied to spending scripts"
                patch
        else pure updatedCtx
    AddInputUTXORaw txIn -> do
      let txInfo = scriptContextTxInfo ctx
          updatedInputs = List.cons txIn (txInfoInputs txInfo)
          updatedTxInfo = txInfo {txInfoInputs = updatedInputs}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    -- Mirror the ledger's translation of a transaction validity interval
    -- (cardano-ledger, Cardano.Ledger.Conway.TxInfo.transValidityInterval):
    -- a finite lower bound is closed (PV1.lowerBound), a finite upper bound is
    -- open (PV1.strictUpperBound), and an omitted bound becomes NegInf/PosInf.
    -- The ledger never produces a finite inclusive upper bound or a finite
    -- exclusive lower bound, so this builder cannot express one either.
    SetValidRange fromTime toTime -> do
      let txInfo = scriptContextTxInfo ctx
          newRange =
            Interval
              ( maybe
                  (LowerBound NegInf True)
                  (\t -> LowerBound (Finite t) True)
                  fromTime
              )
              ( maybe
                  (UpperBound PosInf True)
                  (\t -> UpperBound (Finite t) False)
                  toTime
              )
          updatedTxInfo = txInfo {txInfoValidRange = newRange}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    SetValidRangeRaw range -> do
      let txInfo = scriptContextTxInfo ctx
          updatedTxInfo = txInfo {txInfoValidRange = range}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    AddOutputUTXO address value -> do
      let txInfo = scriptContextTxInfo ctx
          newTxOut = TxOut address value NoOutputDatum Nothing
          updatedOutputs = List.cons newTxOut (txInfoOutputs txInfo)
          updatedTxInfo = txInfo {txInfoOutputs = updatedOutputs}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    RemoveOutputUTXO index -> do
      let txInfo = scriptContextTxInfo ctx
          currentOutputs = txInfoOutputs txInfo
          before = List.take (fromIntegral index) currentOutputs
          after = List.drop (fromIntegral $ index + 1) currentOutputs
          updatedOutputs = List.append before after
          updatedTxInfo = txInfo {txInfoOutputs = updatedOutputs}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    SetScriptDatum datum -> do
      case scriptContextScriptInfo ctx of
        SpendingScript txOutRef _ ->
          pure $
            ctx {scriptContextScriptInfo = SpendingScript txOutRef (Just datum)}
        _ ->
          Left $
            PatchApplicationError
              "SetScriptDatum can only be applied to spending scripts"
              patch
    AddOutputUTXOWithDatum address value datum -> do
      let txInfo = scriptContextTxInfo ctx
          newTxOut = TxOut address value (OutputDatum datum) Nothing
          updatedOutputs = List.cons newTxOut (txInfoOutputs txInfo)
          updatedTxInfo = txInfo {txInfoOutputs = updatedOutputs}
      pure $ ctx {scriptContextTxInfo = updatedTxInfo}
    AddOutputUTXORaw txOut -> do
      let txInfo = scriptContextTxInfo ctx
          updatedOutputs = List.cons txOut (txInfoOutputs txInfo)
      pure $ ctx {scriptContextTxInfo = txInfo {txInfoOutputs = updatedOutputs}}
    SetMintingScript policyId ->
      pure $ ctx {scriptContextScriptInfo = MintingScript policyId}
    SetRewardingScript credential ->
      pure $ ctx {scriptContextScriptInfo = RewardingScript credential}
    SetMint mint -> do
      let txInfo = scriptContextTxInfo ctx
      pure $ ctx {scriptContextTxInfo = txInfo {txInfoMint = mint}}
    SetFee fee -> do
      let txInfo = scriptContextTxInfo ctx
      pure $ ctx {scriptContextTxInfo = txInfo {txInfoFee = fee}}
    SetTxId txId -> do
      let txInfo = scriptContextTxInfo ctx
      pure $ ctx {scriptContextTxInfo = txInfo {txInfoId = txId}}
    SetRedeemers redeemers -> do
      let txInfo = scriptContextTxInfo ctx
          entries =
            [(scriptPurpose purpose, redeemer) | (purpose, redeemer) <- redeemers]
      pure $
        ctx
          { scriptContextTxInfo =
              txInfo {txInfoRedeemers = Map.unsafeFromSOPList entries}
          }
  where
    scriptPurpose = \case
      SpendingPurpose txOutRef -> Spending txOutRef
      MintingPurpose policyId -> Minting policyId
      RewardingPurpose credential -> Rewarding credential

-- JSON instances
instance FromJSON BaselineType where
  parseJSON = Json.withText "BaselineType" \t -> case t of
    "spending" -> pure SpendingBaseline
    _ -> fail $ "Unknown baseline type: " <> Text.unpack t
