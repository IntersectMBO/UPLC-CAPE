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

import Prelude

import Control.Monad (foldM)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Json
import Data.Text qualified as Text
import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.V3.MintValue (emptyMintValue)
import PlutusTx.AssocMap qualified as Map

-- | Baseline template types for ScriptContext generation
data BaselineType = Spending
  deriving stock (Show, Eq, Generic)

-- | Individual patch operations that can be applied to a ScriptContext
data PatchOperation
  = AddSignature V3.PubKeyHash
  | SetRedeemer V3.Redeemer
  | SetSpendingUTXO V3.TxOutRef
  | SetValidRange (Maybe V3.POSIXTime) (Maybe V3.POSIXTime)
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
buildScriptContext :: ScriptContextBuilder -> Either BuildError V3.ScriptContext
buildScriptContext ScriptContextBuilder {baselineType, patches} = do
  let baseline = createBaseline baselineType
  applyPatches patches baseline

-- | Create a baseline ScriptContext template
createBaseline :: BaselineType -> V3.ScriptContext
createBaseline Spending =
  V3.ScriptContext
    { V3.scriptContextTxInfo = createMinimalTxInfo
    , V3.scriptContextRedeemer = V3.Redeemer (V3.toBuiltinData ())
    , V3.scriptContextScriptInfo = V3.SpendingScript dummyTxOutRef (Just dummyDatum)
    }
  where
    createMinimalTxInfo :: V3.TxInfo
    createMinimalTxInfo =
      V3.TxInfo
        { V3.txInfoInputs = []
        , V3.txInfoReferenceInputs = []
        , V3.txInfoOutputs = []
        , V3.txInfoFee = V3.Lovelace 0
        , V3.txInfoMint = emptyMintValue
        , V3.txInfoTxCerts = []
        , V3.txInfoWdrl = Map.empty
        , V3.txInfoValidRange = V3.always
        , V3.txInfoSignatories = []
        , V3.txInfoRedeemers = Map.empty
        , V3.txInfoData = Map.empty
        , V3.txInfoId = dummyTxId
        , V3.txInfoVotes = Map.empty
        , V3.txInfoProposalProcedures = []
        , V3.txInfoCurrentTreasuryAmount = Nothing
        , V3.txInfoTreasuryDonation = Nothing
        }

    dummyTxId :: V3.TxId
    dummyTxId = V3.TxId "0000000000000000000000000000000000000000000000000000000000000000"

    dummyTxOutRef :: V3.TxOutRef
    dummyTxOutRef = V3.TxOutRef dummyTxId 0

    dummyDatum :: V3.Datum
    dummyDatum = V3.Datum (V3.toBuiltinData ())

-- | Apply a sequence of patch operations to a ScriptContext
applyPatches ::
  [PatchOperation] -> V3.ScriptContext -> Either BuildError V3.ScriptContext
applyPatches patches scriptContext = foldM applyPatch scriptContext patches

-- | Apply a single patch operation to a ScriptContext
applyPatch ::
  V3.ScriptContext -> PatchOperation -> Either BuildError V3.ScriptContext
applyPatch ctx patch =
  case patch of
    AddSignature pubKeyHash -> do
      let txInfo = V3.scriptContextTxInfo ctx
          updatedTxInfo =
            txInfo
              { V3.txInfoSignatories = pubKeyHash : V3.txInfoSignatories txInfo
              }
      pure $ ctx {V3.scriptContextTxInfo = updatedTxInfo}
    SetRedeemer redeemer -> do
      pure $ ctx {V3.scriptContextRedeemer = redeemer}
    SetSpendingUTXO txOutRef -> do
      case V3.scriptContextScriptInfo ctx of
        V3.SpendingScript _ maybeDatum ->
          pure $
            ctx {V3.scriptContextScriptInfo = V3.SpendingScript txOutRef maybeDatum}
        _ ->
          Left $
            PatchApplicationError
              "SetSpendingUTXO can only be applied to spending scripts"
              patch
    SetValidRange fromTime toTime -> do
      let txInfo = V3.scriptContextTxInfo ctx
          newRange =
            V3.Interval
              ( maybe
                  (V3.LowerBound V3.NegInf True)
                  (\t -> V3.LowerBound (V3.Finite t) True)
                  fromTime
              )
              ( maybe
                  (V3.UpperBound V3.PosInf True)
                  (\t -> V3.UpperBound (V3.Finite t) True)
                  toTime
              )
          updatedTxInfo = txInfo {V3.txInfoValidRange = newRange}
      pure $ ctx {V3.scriptContextTxInfo = updatedTxInfo}

-- JSON instances
instance FromJSON BaselineType where
  parseJSON =
    Json.withText
      "BaselineType"
      ( \t -> case t of
          "spending" -> pure Spending
          _ -> fail $ "Unknown baseline type: " <> Text.unpack t
      )
