{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{- | Cape test suite loading and resolution module

This module provides data types and functions for loading and processing
UPLC-CAPE test suites from JSON files. It supports:

* Loading test suites with shared data structures
* Resolving test inputs (BuiltinData, RawUPLC, ScriptContext)
* Building ScriptContext from specifications with patches
* Reference resolution for shared test data

Example usage:

@
suite <- loadTestSuite "scenarios/fibonacci/cape-tests.json"
input <- resolveTestInput baseDir suite (tcInput testCase)
@
-}
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
  BaselineSpec (..),
  AddressSpec (..),
  DataStructureEntry (..),
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
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as HaskellMap
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import PlutusCore.Data qualified as PLC
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText, renderParseError)
import PlutusCore.Data.Compact.Printer (dataToCompactText)
import PlutusLedgerApi.Data.V3 qualified as V3
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.Read qualified as Read

-- * Data Structure Definitions

{- | Entry in the data_structures section of test configuration.
Represents either BuiltinData or ScriptContext with their JSON values.
-}
data DataStructureEntry
  = -- | BuiltinData encoded as compact text
    --
    --     JSON example:
    --     @
    --     "buyer_pubkey": {
    --       "type": "builtin_data",
    --       "value": "#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    --     }
    --     @
    BuiltinDataEntry AesonTypes.Value
  | -- | ScriptContext specification
    --
    --     JSON example:
    --     @
    --     "successful_deposit": {
    --       "type": "script_context",
    --       "script_context": {
    --         "baseline": "spending",
    --         "patches": [
    --           {"op": "add_signature", "pubkey_hash": "@buyer_pubkey"}
    --         ]
    --       }
    --     }
    --     @
    ScriptContextEntry AesonTypes.Value
  deriving stock (Show)

{- | Complete test suite loaded from cape-tests.json.
Contains version info, shared data structures, and test cases.
-}
data TestSuite = TestSuite
  { tsVersion :: Text
  -- ^ Version string
  --
  --     JSON: @"version": "1.0.0"@
  , tsDescription :: Maybe Text
  -- ^ Optional description
  --
  --     JSON: @"description": "Test cases for fibonacci validator"@
  , tsDataStructures :: Maybe (HaskellMap.Map Text DataStructureEntry)
  -- ^ Shared test data structures
  --
  --     JSON example:
  --     @
  --     "data_structures": {
  --       "input_42": {
  --         "type": "builtin_data",
  --         "value": "42"
  --       },
  --       "baseline_context": {
  --         "type": "script_context",
  --         "script_context": {
  --           "baseline": "spending",
  --           "patches": []
  --         }
  --       }
  --     }
  --     @
  , tsTests :: [TestCase]
  -- ^ List of test cases
  --
  --     JSON: @"tests": [...]@
  }
  deriving stock (Show)

-- | Individual test case specification.
data TestCase = TestCase
  { tcName :: Text
  -- ^ Test case name
  --
  --     JSON: @"name": "deposit_successful"@
  , tcDescription :: Maybe Text
  -- ^ Optional description
  --
  --     JSON: @"description": "Valid deposit with buyer signature"@
  , tcPending :: Maybe Bool
  -- ^ Whether test is marked pending
  --
  --     JSON: @"pending": true@
  , tcInput :: Maybe TestInput
  -- ^ Input specification (optional for error-only tests)
  --
  --     JSON example:
  --     @
  --     "input": {
  --       "type": "script_context",
  --       "script_context": {
  --         "baseline": "@successful_deposit",
  --         "patches": []
  --       }
  --     }
  --     @
  , tcExpected :: ExpectedResult
  -- ^ Expected result specification
  --
  --     JSON examples:
  --     @
  --     "expected": {"type": "value", "content": "(con unit ())"}
  --     "expected": {"type": "error"}
  --     @
  }
  deriving stock (Show)

-- | Test input specification with multiple input methods.
data TestInput = TestInput
  { tiType :: InputType
  -- ^ Type of input data
  --
  --     JSON: @"type": "builtin_data"@
  , tiValue :: Maybe Text
  -- ^ Inline value (mutually exclusive with tiFile)
  --
  --     JSON examples:
  --     @
  --     "value": "42"
  --     "value": "(program 1.1.0 (con integer 42))"
  --     @
  , tiFile :: Maybe FilePath
  -- ^ File path for input data
  --
  --     JSON: @"file": "complex-input.dat"@
  , tiScriptContext :: Maybe ScriptContextSpec
  -- ^ ScriptContext specification
  --
  --     JSON examples:
  --     @
  --     "script_context": {
  --       "baseline": "spending",
  --       "patches": [
  --         {"op": "add_signature", "pubkey_hash": "deadbeef"}
  --       ]
  --     }
  --
  --     "script_context": {
  --       "baseline": "@successful_deposit",
  --       "patches": [
  --         {"op": "set_redeemer", "redeemer": "1"}
  --       ]
  --     }
  --     @
  }
  deriving stock (Show)

{- | Supported input data types for test cases.
Corresponds to the "type" field in JSON.
-}
data InputType
  = -- | BuiltinData encoded as compact text
    --
    --     JSON: @"type": "builtin_data"@
    BuiltinData
  | -- | UPLC program text
    --
    --     JSON: @"type": "uplc"@
    UPLC
  | -- | Constructed ScriptContext
    --
    --     JSON: @"type": "script_context"@
    ScriptContext
  deriving stock (Show)

-- | Specification for ScriptContext baseline.
data BaselineSpec
  = -- | Direct baseline (e.g. Spending)
    --
    --     JSON: @"baseline": "spending"@
    DirectBaseline BaselineType
  | -- | Reference to shared data (e.g. "@successful_deposit")
    --
    --     JSON: @"baseline": "@successful_deposit"@
    ReferencedBaseline Text
  deriving stock (Show)

-- | Specification for constructing ScriptContext with patches.
data ScriptContextSpec = ScriptContextSpec
  { scsBaseline :: BaselineSpec
  -- ^ Starting baseline context
  --
  --     JSON: @"baseline": "spending"@
  , scsPatches :: [PatchOperationSpec]
  -- ^ Patch operations to apply
  --
  --     JSON example:
  --     @
  --     "patches": [
  --       {
  --         "op": "add_signature",
  --         "pubkey_hash": "@buyer_pubkey"
  --       },
  --       {
  --         "op": "set_redeemer",
  --         "redeemer": "0"
  --       }
  --     ]
  --     @
  }
  deriving stock (Show)

-- | Patch operations for modifying ScriptContext.
data PatchOperationSpec
  = -- | Add signature by pubkey hash
    --
    --     JSON examples:
    --     @
    --     {"op": "add_signature", "pubkey_hash": "deadbeefcafe1234567890abcdef"}
    --     {"op": "add_signature", "pubkey_hash": "@buyer_pubkey"}
    --     @
    AddSignatureSpec Text
  | -- | Remove signature by pubkey hash
    --
    --     JSON example:
    --     @
    --     {"op": "remove_signature", "pubkey_hash": "@seller_pubkey"}
    --     @
    RemoveSignatureSpec Text
  | -- | Set redeemer value (BuiltinData as string)
    --
    --     JSON example:
    --     @
    --     {"op": "set_redeemer", "redeemer": "0"}
    --     @
    SetRedeemerSpec AesonTypes.Value
  | -- | Add input UTXO (ref, lovelace, is_own)
    --
    --     JSON example:
    --     @
    --     {
    --       "op": "add_input_utxo",
    --       "utxo_ref": "1234567890abcdef:0",
    --       "lovelace": 75000000,
    --       "is_own_input": true
    --     }
    --     @
    AddInputUTXOSpec Text Integer Bool
  | -- | Set validity range
    --
    --     JSON example:
    --     @
    --     {
    --       "op": "set_valid_range",
    --       "from_time": 1640995200,
    --       "to_time": 1641081600
    --     }
    --     @
    SetValidRangeSpec (Maybe Integer) (Maybe Integer)
  | -- | Add output UTXO to specified address
    --
    --     JSON examples:
    --     @
    --     {"op": "add_output_utxo", "address": {"type": "script"}, "lovelace": 75000000}
    --     {"op": "add_output_utxo", "address": {"type": "pubkey", "pubkey_hash": "@impostor_pubkey"}, "lovelace": 50000000}
    --     @
    AddOutputUTXOSpec AddressSpec Integer
  | -- | Add output UTXO with datum to specified address
    --
    --     JSON example:
    --     @
    --     {"op": "add_output_utxo", "address": {"type": "script"}, "lovelace": 75000000, "datum": "@deposited_escrow_datum"}
    --     @
    AddOutputUTXOWithDatumSpec AddressSpec Integer AesonTypes.Value
  | -- | Remove output UTXO by index
    --
    --     JSON example:
    --     @
    --     {"op": "remove_output_utxo", "index": 0}
    --     @
    RemoveOutputUTXOSpec Int
  | -- | Set script datum (BuiltinData as string or reference)
    --
    --     JSON examples:
    --     @
    --     {"op": "set_script_datum", "datum": "42"}
    --     {"op": "set_script_datum", "datum": "@accepted_escrow_datum"}
    --     @
    SetScriptDatumSpec AesonTypes.Value
  deriving stock (Show, Eq)

-- | Expected result specification for test cases.
data ExpectedResult = ExpectedResult
  { erType :: ResultType
  -- ^ Type of expected result
  --
  --     JSON: @"type": "value"@ or @"type": "error"@
  , erContent :: Maybe Text
  -- ^ Inline expected content
  --
  --     JSON: @"content": "(con unit ())"@
  , erFile :: Maybe FilePath
  -- ^ File path for expected content
  --
  --     JSON: @"file": "expected-fibonacci-result.uplc"@
  }
  deriving stock (Show)

{- | Type of expected test result.
Corresponds to JSON "type" field.
-}
data ResultType
  = -- | Test should succeed with specific value
    --
    --     JSON: @"type": "value"@
    ExpectedValue
  | -- | Test should fail with error
    --
    --     JSON: @"type": "error"@
    ExpectedError
  deriving stock (Show)

{- | Address specification for ScriptContext operations.
Uses structural JSON format with proper type discrimination.
-}
data AddressSpec
  = -- | Script address with explicit script hash
    --
    --     JSON example:
    --     @
    --     "address": {
    --       "type": "script",
    --       "script_hash": "1111111111111111111111111111111111111111111111111111111111"
    --     }
    --     @
    ScriptAddressSpec Text
  | -- | Public key address with pubkey hash
    --
    --     JSON examples:
    --     @
    --     "address": {
    --       "type": "pubkey",
    --       "pubkey_hash": "@impostor_pubkey"
    --     }
    --     "address": {
    --       "type": "pubkey",
    --       "pubkey_hash": "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    --     }
    --     @
    PubkeyAddressSpec Text
  deriving stock (Show, Eq)

-- * Utility Functions

{- | Parse TxOutRef from "txid:index" format.

Example:

@
parseTxOutRef "1234...cdef:0"
-- Right (TxOutRef (TxId "1234...cdef") 0)
@
-}
parseTxOutRef :: Text -> Either String V3.TxOutRef
parseTxOutRef ref = case Text.splitOn ":" ref of
  [txIdText, indexText] -> do
    index <- Read.readEither (toString indexText)
    let txIdBytes = Builtins.toBuiltin (TE.encodeUtf8 txIdText)
    pure $ V3.TxOutRef (V3.TxId txIdBytes) index
  _ -> Left "Invalid TxOutRef format, expected 'txid:index'"

-- * JSON Parsing Instances

instance FromJSON BaselineSpec where
  parseJSON = \case
    Json.String text
      | Text.isPrefixOf "@" text -> pure $ ReferencedBaseline text
      | otherwise -> DirectBaseline <$> Json.parseJSON (Json.String text)
    other -> DirectBaseline <$> Json.parseJSON other

instance FromJSON DataStructureEntry where
  parseJSON = \case
    Json.Object obj -> do
      dataType :: Text <- obj .: "type"
      case dataType of
        "builtin_data" -> BuiltinDataEntry <$> obj .: "value"
        "script_context" -> ScriptContextEntry <$> obj .: "script_context"
        _ -> fail $ "Unknown data structure type: " <> toString dataType
    other ->
      fail $
        "Expected object for data structure entry with 'type' "
          <> "field, got: "
          <> show other

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
  parseJSON = Json.withText "InputType" \case
    "builtin_data" -> pure BuiltinData
    "uplc" -> pure UPLC
    "script_context" -> pure ScriptContext
    t -> fail $ "Unknown input type: " <> toString t

instance FromJSON ExpectedResult where
  parseJSON = withObject "ExpectedResult" \o ->
    ExpectedResult
      <$> o .: "type"
      <*> o .:? "content"
      <*> o .:? "file"

instance FromJSON ResultType where
  parseJSON = Json.withText "ResultType" \case
    "value" -> pure ExpectedValue
    "error" -> pure ExpectedError
    t -> fail $ "Unknown result type: " <> toString t

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
      "remove_signature" -> RemoveSignatureSpec <$> o .: "pubkey_hash"
      "set_redeemer" -> SetRedeemerSpec <$> o .: "redeemer"
      "add_input_utxo" ->
        AddInputUTXOSpec
          <$> o .: "utxo_ref"
          <*> o .: "lovelace"
          <*> o .: "is_own_input"
      "set_valid_range" ->
        SetValidRangeSpec
          <$> o .:? "from_time"
          <*> o .:? "to_time"
      "add_output_utxo" -> do
        address <- o .: "address"
        lovelace <- o .: "lovelace"
        mDatum <- o .:? "datum"
        case mDatum of
          Just datum -> pure $ AddOutputUTXOWithDatumSpec address lovelace datum
          Nothing -> pure $ AddOutputUTXOSpec address lovelace
      "remove_output_utxo" -> RemoveOutputUTXOSpec <$> o .: "index"
      "set_script_datum" -> SetScriptDatumSpec <$> o .: "datum"
      _ -> fail $ "Unknown patch operation: " <> toString op

instance FromJSON AddressSpec where
  parseJSON = withObject "AddressSpec" \o -> do
    addrType :: Text <- o .: "type"
    case addrType of
      "script" -> ScriptAddressSpec <$> o .: "script_hash"
      "pubkey" -> PubkeyAddressSpec <$> o .: "pubkey_hash"
      _ -> fail $ "Unknown address type: " <> toString addrType

-- * Core Functions

{- | Load test suite from JSON file.

Example:

@
suite <- loadTestSuite "scenarios/fibonacci/cape-tests.json"
@
-}
loadTestSuite :: FilePath -> IO TestSuite
loadTestSuite testFile = do
  content <- LBS.readFile testFile
  case Json.eitherDecode content of
    Left err -> die ("Failed to parse test file: " <> err)
    Right suite -> pure suite

{- | Resolve test input with support for script_context type.

Resolves test inputs from various sources:

* Inline values
* External files
* ScriptContext specifications with patches

Example:

@
input <- resolveTestInput baseDir suite testInput
@
-}
resolveTestInput :: FilePath -> TestSuite -> TestInput -> IO Text
resolveTestInput baseDir testSuite testInput =
  case testInput of
    TestInput {tiType = ScriptContext, tiScriptContext = Just spec} -> do
      let dataStructures = fromMaybe HaskellMap.empty (tsDataStructures testSuite)
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

{- | Resolve file references in expected result.

Loads expected results from inline content or external files.

Example:

@
expected <- resolveExpectedResult baseDir (tcExpected testCase)
@
-}
resolveExpectedResult :: FilePath -> ExpectedResult -> IO (Maybe Text)
resolveExpectedResult _ ExpectedResult {erType = ExpectedError} =
  pure Nothing
resolveExpectedResult _ ExpectedResult {erContent = Just content} =
  pure (Just content)
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

{- | Get base directory for resolving relative file paths.

Used to resolve file references in test inputs and expected results.

Example:

@
baseDir <- getTestBaseDir "scenarios/fibonacci/cape-tests.json"
-- Returns "scenarios/fibonacci"
@
-}
getTestBaseDir :: FilePath -> FilePath
getTestBaseDir = takeDirectory

{- | Check if a test case is marked as pending.

Pending tests are skipped during execution.
-}
isPendingTest :: TestCase -> Bool
isPendingTest tc = fromMaybe False (tcPending tc)

-- * Reference Resolution Functions

{- | Resolve @name references to BuiltinData using typed data structures map.

Supports both reference resolution (@name) and literal parsing.

Example:

@
builtinData <- resolveBuiltinDataReference dataStructures "@buyer_pubkey"
@
-}
resolveBuiltinDataReference ::
  HaskellMap.Map Text DataStructureEntry -> Text -> IO V3.BuiltinData
resolveBuiltinDataReference dataStructures text
  | Text.isPrefixOf "@" text && Text.length text > 1 =
      let refName = Text.drop 1 text
       in case HaskellMap.lookup refName dataStructures of
            Just (BuiltinDataEntry (Json.String val)) ->
              case parseBuiltinDataText val of
                Left parseErr ->
                  die
                    ( "Failed to parse BuiltinData for @"
                        <> toString refName
                        <> ": "
                        <> show (renderParseError parseErr)
                    )
                Right builtinData ->
                  pure $ BI.BuiltinData builtinData
            Just (BuiltinDataEntry _) ->
              die
                ( "Reference @"
                    <> toString refName
                    <> " is builtin_data but not a string value"
                )
            Just (ScriptContextEntry _) ->
              die
                ( "Reference @"
                    <> toString refName
                    <> " is script_context, cannot be used as BuiltinData"
                )
            Nothing ->
              die
                ( "Reference @"
                    <> toString refName
                    <> " not found in data_structures"
                )
  | otherwise =
      -- Not a reference, try to parse as literal BuiltinData
      case parseBuiltinDataText text of
        Left parseErr ->
          die
            ( "Failed to parse BuiltinData: "
                <> show (renderParseError parseErr)
            )
        Right builtinData ->
          pure $ BI.BuiltinData builtinData

{- | Resolve @name references in text using typed data structures map.

Returns the resolved text value or the original text if not a reference.

Example:

@
resolvedText <- resolveTextReference dataStructures "@some_reference"
@
-}
resolveTextReference :: HaskellMap.Map Text DataStructureEntry -> Text -> Text
resolveTextReference dataStructures text
  | Text.isPrefixOf "@" text && Text.length text > 1 =
      let refName = Text.drop 1 text
       in case HaskellMap.lookup refName dataStructures of
            Just (BuiltinDataEntry (Json.String val)) -> val
            Just (BuiltinDataEntry _) ->
              error $
                "Reference @"
                  <> refName
                  <> " is builtin_data but not a string value"
            Just (ScriptContextEntry _) ->
              error $
                "Reference @"
                  <> refName
                  <> " is script_context, cannot be used as text"
            Nothing ->
              error $
                "Reference @"
                  <> refName
                  <> " not found in data_structures"
  | otherwise = text

{- | Resolve script context reference.

Looks up a ScriptContextSpec from the data structures map.

Example:

@
spec <- resolveScriptContextReference dataStructures "successful_deposit"
@
-}
resolveScriptContextReference ::
  HaskellMap.Map Text DataStructureEntry -> Text -> Maybe ScriptContextSpec
resolveScriptContextReference dataStructures refName =
  case HaskellMap.lookup refName dataStructures of
    Just (ScriptContextEntry value) ->
      case Json.fromJSON value of
        Json.Success spec -> Just spec
        Json.Error _err ->
          Nothing -- Parse error - invalid script_context reference
    Just _ -> Nothing -- Not a script_context reference
    Nothing -> Nothing -- Reference not found

-- * Patch Operation Conversion

{- | Convert PatchOperationSpec to PatchOperation with reference resolution.

Handles both literal values and @references from data_structures.

Example:

@
patchOp <- convertPatchOperation dataStructures (AddSignatureSpec "@buyer_pubkey")
@
-}
convertPatchOperation ::
  HaskellMap.Map Text DataStructureEntry ->
  PatchOperationSpec ->
  IO PatchOperation
convertPatchOperation dataStructures spec =
  case spec of
    AddSignatureSpec pubKeyHashText -> do
      -- Check if this is a reference to a builtin_data type
      if Text.isPrefixOf "@" pubKeyHashText && Text.length pubKeyHashText > 1
        then do
          let refName = Text.drop 1 pubKeyHashText
          case HaskellMap.lookup refName dataStructures of
            Just (BuiltinDataEntry _) -> do
              -- Reference to builtin_data: resolve and use bytes directly
              resolvedBuiltinData <-
                resolveBuiltinDataReference dataStructures pubKeyHashText
              let coreData = Builtins.builtinDataToData resolvedBuiltinData
              case coreData of
                PLC.B bytestring -> do
                  let pubKeyHashBytes = Builtins.toBuiltin bytestring
                  pure $ AddSignature (V3.PubKeyHash pubKeyHashBytes)
                _ ->
                  die $
                    "Expected bytestring data for PubKeyHash in AddSignature: "
                      <> toString pubKeyHashText
            _ -> do
              -- Reference to non-builtin_data or literal: use text approach
              let resolvedText =
                    resolveTextReference dataStructures pubKeyHashText
                  pubKeyHashBytes =
                    Builtins.toBuiltin (TE.encodeUtf8 resolvedText)
              pure $ AddSignature (V3.PubKeyHash pubKeyHashBytes)
        else do
          -- Literal case: use old UTF-8 encoding approach
          let resolvedText = resolveTextReference dataStructures pubKeyHashText
              pubKeyHashBytes = Builtins.toBuiltin (TE.encodeUtf8 resolvedText)
          pure $ AddSignature (V3.PubKeyHash pubKeyHashBytes)
    RemoveSignatureSpec pubKeyHashText -> do
      -- Check if this is a reference to a builtin_data type
      if Text.isPrefixOf "@" pubKeyHashText && Text.length pubKeyHashText > 1
        then do
          let refName = Text.drop 1 pubKeyHashText
          case HaskellMap.lookup refName dataStructures of
            Just (BuiltinDataEntry _) -> do
              -- Reference to builtin_data: resolve and use bytes directly
              resolvedBuiltinData <-
                resolveBuiltinDataReference dataStructures pubKeyHashText
              let coreData = Builtins.builtinDataToData resolvedBuiltinData
              case coreData of
                PLC.B bytestring -> do
                  let pubKeyHashBytes = Builtins.toBuiltin bytestring
                  pure $ RemoveSignature (V3.PubKeyHash pubKeyHashBytes)
                _ ->
                  die $
                    "Expected bytestring data for PubKeyHash in RemoveSignature: "
                      <> toString pubKeyHashText
            _ -> do
              -- Reference to non-builtin_data or literal: use text approach
              let resolvedText =
                    resolveTextReference dataStructures pubKeyHashText
                  pubKeyHashBytes =
                    Builtins.toBuiltin (TE.encodeUtf8 resolvedText)
              pure $ RemoveSignature (V3.PubKeyHash pubKeyHashBytes)
        else do
          -- Literal case: use old UTF-8 encoding approach
          let resolvedText = resolveTextReference dataStructures pubKeyHashText
              pubKeyHashBytes = Builtins.toBuiltin (TE.encodeUtf8 resolvedText)
          pure $ RemoveSignature (V3.PubKeyHash pubKeyHashBytes)
    SetRedeemerSpec redeemerValue -> do
      -- Convert JSON Value to BuiltinData (expects string with builtin data encoding)
      case redeemerValue of
        Json.String dataText ->
          case parseBuiltinDataText dataText of
            Left parseErr ->
              die $
                "Failed to parse redeemer BuiltinData: "
                  <> show (renderParseError parseErr)
            Right builtinData ->
              pure $ SetRedeemer (V3.Redeemer (BI.BuiltinData builtinData))
        other ->
          die $
            "Redeemer must be a string with BuiltinData encoding, got: "
              <> show other
    AddInputUTXOSpec utxoRefText lovelaceAmount isOwnInput -> do
      let resolvedUtxoRef = resolveTextReference dataStructures utxoRefText
      case parseTxOutRef resolvedUtxoRef of
        Left parseErr ->
          die ("Failed to parse UTXO reference: " <> show parseErr)
        Right txOutRef -> do
          let value = V3.singleton V3.adaSymbol V3.adaToken lovelaceAmount
          pure $ AddInputUTXO txOutRef value isOwnInput
    SetValidRangeSpec fromTime toTime -> do
      let fromPosix = fmap V3.POSIXTime fromTime
          toPosix = fmap V3.POSIXTime toTime
      pure $ SetValidRange fromPosix toPosix
    AddOutputUTXOSpec addressSpec lovelaceAmount -> do
      let value = V3.singleton V3.adaSymbol V3.adaToken lovelaceAmount
      address <- parseAddressSpec dataStructures addressSpec
      pure $ AddOutputUTXO address value
    AddOutputUTXOWithDatumSpec addressSpec lovelaceAmount datumValue -> do
      let value = V3.singleton V3.adaSymbol V3.adaToken lovelaceAmount
      address <- parseAddressSpec dataStructures addressSpec
      -- Convert JSON Value to BuiltinData with reference resolution support
      case datumValue of
        Json.String dataText -> do
          -- Check if this is a reference to a builtin_data type
          if Text.isPrefixOf "@" dataText && Text.length dataText > 1
            then do
              -- Reference case: resolve from data structures
              resolvedBuiltinData <-
                resolveBuiltinDataReference dataStructures dataText
              let datum = V3.Datum resolvedBuiltinData
              pure $ AddOutputUTXOWithDatum address value datum
            else do
              -- Direct BuiltinData case
              case parseBuiltinDataText dataText of
                Left parseErr ->
                  die ("Failed to parse BuiltinData: " <> toString (renderParseError parseErr))
                Right parsedBuiltinData -> do
                  let datum = V3.Datum (BI.BuiltinData parsedBuiltinData)
                  pure $ AddOutputUTXOWithDatum address value datum
        other ->
          die $
            "Datum must be a string with BuiltinData encoding, got: "
              <> show other
    RemoveOutputUTXOSpec index -> do
      pure $ RemoveOutputUTXO index
    SetScriptDatumSpec datumValue -> do
      -- Convert JSON Value to BuiltinData with reference resolution support
      case datumValue of
        Json.String dataText -> do
          -- Check if this is a reference to a builtin_data type
          if Text.isPrefixOf "@" dataText && Text.length dataText > 1
            then do
              -- Reference case: resolve from data structures
              resolvedBuiltinData <-
                resolveBuiltinDataReference dataStructures dataText
              pure $ SetScriptDatum (V3.Datum resolvedBuiltinData)
            else do
              -- Literal case: parse as BuiltinData
              case parseBuiltinDataText dataText of
                Left parseErr ->
                  die $
                    "Failed to parse script datum BuiltinData: "
                      <> show (renderParseError parseErr)
                Right builtinData ->
                  pure $ SetScriptDatum (V3.Datum (BI.BuiltinData builtinData))
        other ->
          die $
            "Script datum must be a string with BuiltinData encoding, got: "
              <> show other

{- | Parse AddressSpec into Address with reference resolution.

Supports structural address format:
- ScriptAddressSpec: Script address (uses standard all-1s script hash)
- PubkeyAddressSpec: Public key address with pubkey hash (supports @references and hex literals)
-}
parseAddressSpec ::
  HaskellMap.Map Text DataStructureEntry -> AddressSpec -> IO V3.Address
parseAddressSpec dataStructures addressSpec = case addressSpec of
  ScriptAddressSpec scriptHashText -> do
    -- Use OverloadedStrings directly like ScriptContextBuilder does
    pure $
      V3.Address
        (V3.ScriptCredential (V3.ScriptHash (fromString (toString scriptHashText))))
        Nothing
  PubkeyAddressSpec pubkeyHashText -> do
    -- Check if this is a reference to a builtin_data type
    if Text.isPrefixOf "@" pubkeyHashText && Text.length pubkeyHashText > 1
      then do
        let refName = Text.drop 1 pubkeyHashText
        case HaskellMap.lookup refName dataStructures of
          Just (BuiltinDataEntry _) -> do
            -- Reference to builtin_data: resolve and use bytes directly
            resolvedBuiltinData <-
              resolveBuiltinDataReference dataStructures pubkeyHashText
            let coreData = Builtins.builtinDataToData resolvedBuiltinData
            case coreData of
              PLC.B bytestring -> do
                let pubKeyHashBytes = Builtins.toBuiltin bytestring
                pure $ V3.Address (V3.PubKeyCredential (V3.PubKeyHash pubKeyHashBytes)) Nothing
              _ ->
                die $
                  "Expected bytestring data for PubKeyHash in address: "
                    <> toString pubkeyHashText
          _ ->
            die $
              "Address reference @"
                <> toString refName
                <> " not found or not a builtin_data type"
      else do
        -- Literal hex string: use OverloadedStrings directly for consistency
        pure $
          V3.Address
            (V3.PubKeyCredential (V3.PubKeyHash (fromString (toString pubkeyHashText))))
            Nothing

{- | Convert ScriptContextSpec to ScriptContextBuilder with reference resolution.

Handles both direct baselines and referenced baselines with recursive resolution.
Combines inherited patches from references with local patches.

Example:

@
builder <- convertScriptContextSpec dataStructures contextSpec
@
-}
convertScriptContextSpec ::
  HaskellMap.Map Text DataStructureEntry ->
  ScriptContextSpec ->
  IO ScriptContextBuilder
convertScriptContextSpec
  dataStructures
  ScriptContextSpec {scsBaseline, scsPatches} = do
    -- Handle baseline references
    (finalBaseline, inheritedPatches) <- case scsBaseline of
      DirectBaseline baseline -> pure (baseline, [])
      ReferencedBaseline refText -> do
        let refName = Text.drop 1 refText -- Remove '@' prefix
        case resolveScriptContextReference dataStructures refName of
          Just referencedSpec -> do
            -- Recursively resolve the referenced spec
            ScriptContextBuilder refBaseline refPatches <-
              convertScriptContextSpec dataStructures referencedSpec
            pure (refBaseline, refPatches)
          Nothing ->
            die $
              "Script context reference @"
                <> toString refName
                <> " not found or not a script_context type"

    -- Convert patches and combine inherited with local patches
    convertedInheritedPatches <- pure inheritedPatches
    convertedPatches <- mapM (convertPatchOperation dataStructures) scsPatches
    pure $
      ScriptContextBuilder
        finalBaseline
        (convertedInheritedPatches <> convertedPatches)

{- | Resolve script context input and convert to BuiltinData text.

Builds ScriptContext from specification and converts to compact text format.

Example:

@
contextText <- resolveScriptContextInput dataStructures contextSpec
@
-}
resolveScriptContextInput ::
  HaskellMap.Map Text DataStructureEntry -> ScriptContextSpec -> IO Text
resolveScriptContextInput dataStructures spec = do
  builder <- convertScriptContextSpec dataStructures spec
  case buildScriptContext builder of
    Left err -> die ("Failed to build ScriptContext: " <> show err)
    Right scriptContext ->
      pure $
        dataToCompactText $
          Builtins.builtinDataToData $
            V3.toBuiltinData scriptContext
