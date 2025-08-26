module Cape.Tests (
  TestSuite (..),
  TestCase (..),
  TestInput (..),
  InputType (..),
  ScriptContextSpec (..),
  PatchOperationSpec (..),
  ExpectedResult (..),
  ResultType (..),
  BaselineType (..),
  loadTestSuite,
  resolveTestInput,
  resolveExpectedResult,
  getTestBaseDir,
  isPendingTest,
) where

import Prelude

import Cape.ScriptContextBuilder
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Json
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText, renderParseError)
import PlutusCore.Data.Compact.Printer (dataToCompactText)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

-- | Test specification data types
data TestSuite = TestSuite
  { tsVersion :: Text
  , tsDescription :: Maybe Text
  , tsDataStructures :: Maybe (Map Text Value)
  , tsTests :: [TestCase]
  }
  deriving stock (Show, Generic)

data TestCase = TestCase
  { tcName :: Text
  , tcDescription :: Maybe Text
  , tcPending :: Maybe Bool
  , tcInput :: Maybe TestInput
  , tcExpected :: ExpectedResult
  }
  deriving stock (Show, Generic)

data TestInput = TestInput
  { tiType :: InputType
  , tiValue :: Maybe Text
  , tiFile :: Maybe FilePath
  , tiScriptContext :: Maybe ScriptContextSpec
  }
  deriving stock (Show, Generic)

data InputType
  = BuiltinData
  | RawUPLC
  | ScriptContext
  deriving stock (Show, Generic)

data ScriptContextSpec = ScriptContextSpec
  { scsBaseline :: BaselineType
  , scsPatches :: [PatchOperationSpec]
  }
  deriving stock (Show, Generic)

data PatchOperationSpec
  = AddSignatureSpec Text
  | SetRedeemerSpec Value
  | SetSpendingUTXOSpec Text Integer
  | SetValidRangeSpec (Maybe Integer) (Maybe Integer)
  deriving stock (Show, Generic)

data ExpectedResult = ExpectedResult
  { erType :: ResultType
  , erContent :: Maybe Text
  , erFile :: Maybe FilePath
  }
  deriving stock (Show, Generic)

data ResultType
  = ExpectedValue
  | ExpectedError
  deriving stock (Show, Generic)

-- JSON instances
instance FromJSON TestSuite where
  parseJSON = withObject "TestSuite" \o ->
    TestSuite
      <$> o .: "version"
      <*> o .:? "description"
      <*> o .:? "data_structures"
      <*> o .: "tests"

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" \o ->
    TestCase
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .:? "pending"
      <*> o .:? "input"
      <*> o .: "expected"

instance FromJSON TestInput where
  parseJSON = withObject "TestInput" \o ->
    TestInput
      <$> o .: "type"
      <*> o .:? "value"
      <*> o .:? "file"
      <*> o .:? "script_context"

instance FromJSON InputType where
  parseJSON = Json.withText "InputType" \t -> case t of
    "builtin_data" -> pure BuiltinData
    "raw_uplc" -> pure RawUPLC
    "script_context" -> pure ScriptContext
    _ -> fail $ "Unknown input type: " <> Text.unpack t

instance FromJSON ExpectedResult where
  parseJSON = withObject "ExpectedResult" \o ->
    ExpectedResult
      <$> o .: "type"
      <*> o .:? "content"
      <*> o .:? "file"

instance FromJSON ResultType where
  parseJSON = Json.withText "ResultType" \t -> case t of
    "value" -> pure ExpectedValue
    "error" -> pure ExpectedError
    _ -> fail $ "Unknown result type: " <> Text.unpack t

instance FromJSON ScriptContextSpec where
  parseJSON = withObject "ScriptContextSpec" \o ->
    ScriptContextSpec
      <$> o .: "baseline"
      <*> o .: "patches"

instance FromJSON PatchOperationSpec where
  parseJSON = withObject "PatchOperationSpec" \o -> do
    op :: Text <- o .: "op"
    case op of
      "add_signature" -> AddSignatureSpec <$> o .: "pubkey_hash"
      "set_redeemer" -> SetRedeemerSpec <$> o .: "redeemer"
      "set_spending_utxo" -> SetSpendingUTXOSpec <$> o .: "tx_id" <*> o .: "tx_index"
      "set_valid_range" -> SetValidRangeSpec <$> o .:? "from_time" <*> o .:? "to_time"
      _ -> fail $ "Unknown patch operation: " <> Text.unpack op

-- | Load test suite from JSON file
loadTestSuite :: FilePath -> IO TestSuite
loadTestSuite testFile = do
  content <- LBS.readFile testFile
  case Json.eitherDecode content of
    Left err -> die ("Failed to parse test file: " <> err)
    Right suite -> pure suite

-- | Resolve test input with support for script_context type
resolveTestInput :: FilePath -> TestSuite -> TestInput -> IO Text
resolveTestInput baseDir testSuite testInput =
  case testInput of
    TestInput {tiType = ScriptContext, tiScriptContext = Just spec} -> do
      let dataStructures = fromMaybe Map.empty (tsDataStructures testSuite)
      resolveScriptContextInput dataStructures spec
    TestInput {tiType = ScriptContext, tiScriptContext = Nothing} ->
      die "ScriptContext input type requires script_context specification"
    TestInput {tiValue = Just val} -> pure val
    TestInput {tiFile = Just file} -> do
      let fullPath = baseDir </> file
      exists <- doesFileExist fullPath
      if not exists
        then die ("Input file not found: " <> fullPath)
        else do
          bs <- readFileBS fullPath
          case decodeUtf8' bs of
            Left _ -> die ("Invalid UTF-8 in file: " <> fullPath)
            Right t -> pure t
    _ ->
      die "Test input must have either 'value', 'file', or 'script_context'"

-- | Resolve file references in expected result
resolveExpectedResult :: FilePath -> ExpectedResult -> IO (Maybe Text)
resolveExpectedResult _ ExpectedResult {erType = ExpectedError} = pure Nothing
resolveExpectedResult _ ExpectedResult {erContent = Just content} = pure (Just content)
resolveExpectedResult baseDir ExpectedResult {erFile = Just file} = do
  let fullPath = baseDir </> file
  exists <- doesFileExist fullPath
  if not exists
    then die ("Expected result file not found: " <> fullPath)
    else do
      bs <- readFileBS fullPath
      case decodeUtf8' bs of
        Left _ -> die ("Invalid UTF-8 in file: " <> fullPath)
        Right t -> pure (Just t)
resolveExpectedResult _ _ =
  die "Expected result of type 'value' must have either 'content' or 'file'"

-- | Get base directory for resolving relative file paths
getTestBaseDir :: FilePath -> FilePath
getTestBaseDir = takeDirectory

-- | Check if a test case is marked as pending
isPendingTest :: TestCase -> Bool
isPendingTest tc = fromMaybe False (tcPending tc)

-- | Resolve @name references in text using data structures map
resolveReferences :: Map Text Value -> Text -> Text
resolveReferences dataStructures text
  | Text.isPrefixOf "@" text && Text.length text > 1 =
      let refName = Text.drop 1 text
       in case Map.lookup refName dataStructures of
            Just (Json.String val) -> val
            Just _val ->
              error
                ( toText
                    ( "Reference @"
                        <> Text.unpack refName
                        <> " is not a string"
                    )
                )
            Nothing ->
              error
                ( toText
                    ( "Reference @"
                        <> Text.unpack refName
                        <> " not found in data_structures"
                    )
                )
  | otherwise = text

-- | Convert PatchOperationSpec to PatchOperation with reference resolution
convertPatchOperation ::
  Map Text Value -> PatchOperationSpec -> IO PatchOperation
convertPatchOperation dataStructures spec =
  case spec of
    AddSignatureSpec pubKeyHashText -> do
      let resolvedText = resolveReferences dataStructures pubKeyHashText
          pubKeyHashBytes = Builtins.toBuiltin (TE.encodeUtf8 resolvedText)
      pure $ AddSignature (V3.PubKeyHash pubKeyHashBytes)
    SetRedeemerSpec redeemerValue -> do
      -- Convert JSON Value to BuiltinData (expects string with builtin data encoding)
      case redeemerValue of
        Json.String dataText ->
          case parseBuiltinDataText dataText of
            Left parseErr ->
              die
                ( "Failed to parse redeemer BuiltinData: "
                    <> show (renderParseError parseErr)
                )
            Right builtinData ->
              pure $ SetRedeemer (V3.Redeemer (BI.BuiltinData builtinData))
        other ->
          die ("Redeemer must be a string with BuiltinData encoding, got: " <> show other)
    SetSpendingUTXOSpec txIdText txIndex -> do
      let resolvedTxId = resolveReferences dataStructures txIdText
          txIdBytes = Builtins.toBuiltin (TE.encodeUtf8 resolvedTxId)
      pure $ SetSpendingUTXO (V3.TxOutRef (V3.TxId txIdBytes) txIndex)
    SetValidRangeSpec fromTime toTime -> do
      let fromPosix = fmap V3.POSIXTime fromTime
          toPosix = fmap V3.POSIXTime toTime
      pure $ SetValidRange fromPosix toPosix

-- | Convert ScriptContextSpec to ScriptContextBuilder with reference resolution
convertScriptContextSpec ::
  Map Text Value -> ScriptContextSpec -> IO ScriptContextBuilder
convertScriptContextSpec dataStructures ScriptContextSpec {scsBaseline, scsPatches} = do
  convertedPatches <- mapM (convertPatchOperation dataStructures) scsPatches
  pure $ ScriptContextBuilder scsBaseline convertedPatches

-- | Resolve script context input and convert to BuiltinData text
resolveScriptContextInput :: Map Text Value -> ScriptContextSpec -> IO Text
resolveScriptContextInput dataStructures spec = do
  builder <- convertScriptContextSpec dataStructures spec
  case buildScriptContext builder of
    Left err -> die ("Failed to build ScriptContext: " <> show err)
    Right scriptContext -> do
      let builtinData = V3.toBuiltinData scriptContext
          -- Convert BuiltinData to PlutusCore.Data
          coreData = Builtins.builtinDataToData builtinData
      -- Convert PlutusCore.Data to compact text representation
      pure $ dataToCompactText coreData
