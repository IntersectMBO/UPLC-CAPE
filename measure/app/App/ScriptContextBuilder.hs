module App.ScriptContextBuilder 
  ( ScriptContextSpec(..)
  , BaselineTemplate(..)
  , PatchOperation(..)
  , TxOutRef(..)
  , POSIXTimeRange(..)
  , buildScriptContext
  , resolvePatchReferences
  ) where

import Prelude

import Data.Aeson (FromJSON (..), withObject, withText, (.:), (.:?))
import Data.Aeson qualified as Json
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusTx.Builtins (BuiltinData, toBuiltinData)
import Text.Read (readMaybe)

-- | ScriptContext DSL specification
data ScriptContextSpec = ScriptContextSpec
  { scsBaseline :: BaselineTemplate
  , scsPatches :: [PatchOperation]
  , scsDataStructures :: Maybe (Map.Map Text Json.Value)
  }
  deriving (Show)

-- | Baseline ScriptContext templates
data BaselineTemplate
  = SpendingBaseline
  | MintingBaseline  
  | RewardingBaseline
  | CertifyingBaseline
  | VotingBaseline
  | ProposingBaseline
  deriving (Show)

-- | Patch operations for modifying ScriptContext
data PatchOperation
  = AddSignature Text  -- pubkey hash
  | SetRedeemer Json.Value
  | SetSpendingUtxo TxOutRef
  | SetValidRange POSIXTimeRange
  deriving (Show)

-- | Transaction output reference
data TxOutRef = TxOutRef
  { torTxId :: Text
  , torOutputIndex :: Integer
  }
  deriving (Show)

-- | POSIX time range
data POSIXTimeRange = POSIXTimeRange
  { ptrFrom :: Maybe Integer
  , ptrTo :: Maybe Integer
  }
  deriving (Show)

-- JSON instances
instance FromJSON ScriptContextSpec where
  parseJSON = withObject "ScriptContextSpec" $ \o ->
    ScriptContextSpec
      <$> o .: "baseline"
      <*> (o .:? "patches" >>= \case
            Nothing -> pure []
            Just patches -> parseJSON patches)
      <*> o .:? "data_structures"

instance FromJSON BaselineTemplate where
  parseJSON = withText "BaselineTemplate" $ \t -> case t of
    "spending" -> pure SpendingBaseline
    "minting" -> pure MintingBaseline
    "rewarding" -> pure RewardingBaseline
    "certifying" -> pure CertifyingBaseline
    "voting" -> pure VotingBaseline
    "proposing" -> pure ProposingBaseline
    _ -> fail $ "Unknown baseline template: " <> Text.unpack t

instance FromJSON PatchOperation where
  parseJSON = withObject "PatchOperation" $ \o -> do
    op <- o .: "op"
    case op of
      "add_signature" -> AddSignature <$> o .: "pubkey_hash"
      "set_redeemer" -> SetRedeemer <$> o .: "value"
      "set_spending_utxo" -> do
        txId <- o .: "tx_id"
        outputIndex <- o .: "output_index"
        pure $ SetSpendingUtxo (TxOutRef txId outputIndex)
      "set_valid_range" -> do
        from <- o .:? "from"
        to <- o .:? "to"
        pure $ SetValidRange (POSIXTimeRange from to)
      _ -> fail $ "Unknown patch operation: " <> Text.unpack op

instance FromJSON TxOutRef where
  parseJSON = withObject "TxOutRef" $ \o ->
    TxOutRef
      <$> o .: "tx_id"
      <*> o .: "output_index"

-- | Build a ScriptContext from the DSL specification
buildScriptContext :: ScriptContextSpec -> Either Text BuiltinData
buildScriptContext spec = do
  -- Resolve data structure references in patches
  resolvedPatches <- mapM (resolvePatchReferences (scsDataStructures spec)) (scsPatches spec)
  
  -- Start with baseline template - for now create a simple data structure
  baseData <- createBaselineData (scsBaseline spec)
  
  -- Apply patches in order (simplified for now)
  finalData <- foldM applyPatchToData baseData resolvedPatches
  
  -- Return the BuiltinData
  pure finalData

-- | Create minimal baseline data structure
-- This is a simplified representation until we get the V3 API working
createBaselineData :: BaselineTemplate -> Either Text BuiltinData
createBaselineData SpendingBaseline = do
  -- Create a minimal spending ScriptContext as BuiltinData
  -- For now, represent as a simple constructor: 0(txInfo redeemer purpose)
  -- where each component is also simplified
  let
    -- Empty transaction info (simplified)
    txInfoData = toBuiltinData ()  -- Placeholder
    
    -- Default redeemer
    redeemerData = toBuiltinData ()
    
    -- Spending purpose
    purposeData = toBuiltinData (0 :: Integer)  -- 0 for Spending
    
    -- Combine into ScriptContext
    scriptContextData = toBuiltinData (txInfoData, redeemerData, purposeData)
  
  pure scriptContextData

createBaselineData template = 
  Left $ "Baseline template not yet implemented: " <> Text.pack (show template)

-- | Apply a patch operation to BuiltinData (simplified)
applyPatchToData :: BuiltinData -> PatchOperation -> Either Text BuiltinData
applyPatchToData baseData patch = case patch of
  AddSignature pubkeyHash -> do
    -- For now, just validate the format and return the original data
    _ <- validatePubKeyHash pubkeyHash
    pure baseData
    
  SetRedeemer value -> do
    -- Convert JSON value to BuiltinData
    _redeemerData <- jsonToBuiltinData value
    -- For now, return the original data
    pure baseData
    
  SetSpendingUtxo _utxoRef -> do
    -- For now, just return the original data
    pure baseData
    
  SetValidRange _timeRange -> do
    -- For now, just return the original data
    pure baseData

-- Helper functions
validatePubKeyHash :: Text -> Either Text ()
validatePubKeyHash txt = 
  if Text.take 1 txt == "#" && Text.length txt == 57
    then pure ()
    else Left $ "Invalid pubkey hash format: " <> txt

-- Convert JSON value to BuiltinData (simplified for now)
jsonToBuiltinData :: Json.Value -> Either Text BuiltinData
jsonToBuiltinData (Json.Number n) = 
  pure $ toBuiltinData (floor n :: Integer)
jsonToBuiltinData (Json.String s) = 
  pure $ toBuiltinData s
jsonToBuiltinData (Json.Bool b) = 
  pure $ toBuiltinData b
jsonToBuiltinData Json.Null = 
  pure $ toBuiltinData ()
jsonToBuiltinData _ = 
  Left "Complex JSON structures not yet supported for redeemer values"

-- | Resolve data structure references in patch operations
resolvePatchReferences :: Maybe (Map.Map Text Json.Value) -> PatchOperation -> Either Text PatchOperation
resolvePatchReferences dataStructures patch = case patch of
  AddSignature pubkeyHash -> do
    resolvedHash <- resolveReference dataStructures pubkeyHash
    pure $ AddSignature resolvedHash
    
  SetRedeemer value -> do
    -- For JSON values in redeemers, we don't resolve references yet
    pure $ SetRedeemer value
    
  SetSpendingUtxo utxoRef -> do
    resolvedTxId <- resolveReference dataStructures (torTxId utxoRef)
    -- Note: output_index could also be a reference, but for simplicity keeping as integer for now
    pure $ SetSpendingUtxo (utxoRef { torTxId = resolvedTxId })
    
  SetValidRange timeRange -> do
    resolvedFrom <- case ptrFrom timeRange of
      Nothing -> pure Nothing
      Just fromVal -> do
        fromText <- case fromVal of
          -- If it's a reference (starts with @), resolve it
          _ -> pure $ Text.pack (show fromVal)  -- For now, just convert to text
        resolved <- resolveReferenceOptional dataStructures fromText
        case resolved of
          Just val -> case readMaybe (Text.unpack val) of
            Nothing -> Left $ "Invalid timestamp value: " <> val
            Just ts -> pure (Just ts)
          Nothing -> pure Nothing
          
    resolvedTo <- case ptrTo timeRange of
      Nothing -> pure Nothing
      Just toVal -> do
        toText <- pure $ Text.pack (show toVal)
        resolved <- resolveReferenceOptional dataStructures toText
        case resolved of
          Just val -> case readMaybe (Text.unpack val) of
            Nothing -> Left $ "Invalid timestamp value: " <> val
            Just ts -> pure (Just ts)
          Nothing -> pure Nothing
    
    pure $ SetValidRange (POSIXTimeRange resolvedFrom resolvedTo)

-- | Resolve a single reference (handles @name syntax)
resolveReference :: Maybe (Map.Map Text Json.Value) -> Text -> Either Text Text
resolveReference Nothing ref = pure ref  -- No data structures defined
resolveReference (Just dataStructures) ref = do
  if Text.take 1 ref == "@"
    then do
      let refName = Text.drop 1 ref
      case Map.lookup refName dataStructures of
        Nothing -> Left $ "Data structure reference not found: " <> refName
        Just (Json.String val) -> pure val
        Just val -> Left $ "Data structure reference must be string, got: " <> Text.pack (show val)
    else pure ref

-- | Resolve optional reference
resolveReferenceOptional :: Maybe (Map.Map Text Json.Value) -> Text -> Either Text (Maybe Text)
resolveReferenceOptional dataStructures ref = do
  if Text.take 1 ref == "@"
    then do
      resolved <- resolveReference dataStructures ref
      pure (Just resolved)
    else pure (Just ref)