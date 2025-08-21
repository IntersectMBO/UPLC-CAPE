module App.Tests where

import Prelude

import Control.Exception (throwIO)
import Control.Exception qualified as E
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (userError)

-- | Test specification data types
data TestSuite = TestSuite
  { tsVersion :: Text
  , tsDescription :: Maybe Text
  , tsTests :: [TestCase]
  }
  deriving (Show, Generic)

data TestCase = TestCase
  { tcName :: Text
  , tcDescription :: Maybe Text
  , tcInput :: Maybe TestInput
  , tcExpected :: ExpectedResult
  }
  deriving (Show, Generic)

data TestInput = TestInput
  { tiType :: InputType
  , tiValue :: Maybe Text
  , tiFile :: Maybe FilePath
  }
  deriving (Show, Generic)

data InputType
  = BuiltinData
  | RawUPLC
  | ScriptContext
  deriving (Show, Generic)

data ExpectedResult = ExpectedResult
  { erType :: ResultType
  , erContent :: Maybe Text
  , erFile :: Maybe FilePath
  }
  deriving (Show, Generic)

data ResultType
  = ExpectedValue
  | ExpectedError
  deriving (Show, Generic)

-- JSON instances
instance FromJSON TestSuite where
  parseJSON = withObject "TestSuite" $ \o ->
    TestSuite
      <$> o .: "version"
      <*> o .:? "description"
      <*> o .: "tests"

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" $ \o ->
    TestCase
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .:? "input"
      <*> o .: "expected"

instance FromJSON TestInput where
  parseJSON = withObject "TestInput" $ \o ->
    TestInput
      <$> o .: "type"
      <*> o .:? "value"
      <*> o .:? "file"

instance FromJSON InputType where
  parseJSON = Json.withText "InputType" $ \t -> case t of
    "builtin_data" -> pure BuiltinData
    "raw_uplc" -> pure RawUPLC
    "script_context" -> pure ScriptContext
    _ -> fail $ "Unknown input type: " <> Text.unpack t

instance FromJSON ExpectedResult where
  parseJSON = withObject "ExpectedResult" $ \o ->
    ExpectedResult
      <$> o .: "type"
      <*> o .:? "content"
      <*> o .:? "file"

instance FromJSON ResultType where
  parseJSON = Json.withText "ResultType" $ \t -> case t of
    "value" -> pure ExpectedValue
    "error" -> pure ExpectedError
    _ -> fail $ "Unknown result type: " <> Text.unpack t

-- | Load test suite from JSON file
loadTestSuite :: FilePath -> IO TestSuite
loadTestSuite testFile = do
  content <- LBS.readFile testFile
  case Json.eitherDecode content of
    Left err -> throwIO (userError $ "Failed to parse test file: " <> err)
    Right suite -> pure suite

-- | Resolve file references in test input
resolveTestInput :: FilePath -> TestInput -> IO Text
resolveTestInput baseDir TestInput {tiValue = Just val} = pure val
resolveTestInput baseDir TestInput {tiFile = Just file} = do
  let fullPath = baseDir </> file
  exists <- doesFileExist fullPath
  if not exists
    then throwIO (userError $ "Input file not found: " <> fullPath)
    else readFileText fullPath
resolveTestInput _ _ = throwIO (userError "Test input must have either 'value' or 'file'")

-- | Resolve file references in expected result
resolveExpectedResult :: FilePath -> ExpectedResult -> IO (Maybe Text)
resolveExpectedResult _ ExpectedResult {erType = ExpectedError} = pure Nothing
resolveExpectedResult _ ExpectedResult {erContent = Just content} = pure (Just content)
resolveExpectedResult baseDir ExpectedResult {erFile = Just file} = do
  let fullPath = baseDir </> file
  exists <- doesFileExist fullPath
  if not exists
    then throwIO (userError $ "Expected result file not found: " <> fullPath)
    else Just <$> readFileText fullPath
resolveExpectedResult _ _ =
  throwIO
    ( userError "Expected result of type 'value' must have either 'content' or 'file'"
    )

-- | Get base directory for resolving relative file paths
getTestBaseDir :: FilePath -> FilePath
getTestBaseDir = takeDirectory
