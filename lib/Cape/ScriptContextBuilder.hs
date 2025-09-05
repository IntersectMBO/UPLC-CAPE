module Cape.ScriptContextBuilder (
  ScriptContextBuilder (..),
  BaselineType (..),
  PatchOperation (..),
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
  | AddInputUTXO TxOutRef Value Bool
  | SetValidRange (Maybe POSIXTime) (Maybe POSIXTime)
  | AddOutputUTXO Address Value
  | RemoveOutputUTXO Int
  | SetScriptDatum Datum
  | AddOutputUTXOWithDatum Address Value Datum
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
    AddInputUTXO txOutRef value isOwnInput -> do
      let txInfo = scriptContextTxInfo ctx
          -- Use script address for script inputs, dummy address for regular inputs
          inputAddr =
            if isOwnInput
              then
                Address
                  ( ScriptCredential
                      (ScriptHash "1111111111111111111111111111111111111111111111111111111111")
                  )
                  Nothing
              else Address (PubKeyCredential (PubKeyHash "")) Nothing
          newTxIn = TxInInfo txOutRef (TxOut inputAddr value NoOutputDatum Nothing)
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
                  (\t -> UpperBound (Finite t) True)
                  toTime
              )
          updatedTxInfo = txInfo {txInfoValidRange = newRange}
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

-- JSON instances
instance FromJSON BaselineType where
  parseJSON = Json.withText "BaselineType" \t -> case t of
    "spending" -> pure SpendingBaseline
    _ -> fail $ "Unknown baseline type: " <> Text.unpack t
