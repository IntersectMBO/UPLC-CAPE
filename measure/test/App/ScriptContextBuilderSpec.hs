module App.ScriptContextBuilderSpec where

import Prelude

import App.ScriptContextBuilder
import Data.Aeson qualified as Json
import Data.Map qualified as Map
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

spec :: Spec
spec = describe "ScriptContextBuilder" $ do
  
  describe "JSON parsing" $ do
    it "parses basic spending baseline" $ do
      let jsonText = "{\"baseline\": \"spending\", \"patches\": []}"
      case Json.eitherDecodeStrict (encodeUtf8 jsonText) of
        Left err -> expectationFailure $ "Parse error: " <> err
        Right (spec :: ScriptContextSpec) -> do
          scsBaseline spec `shouldBe` SpendingBaseline
          scsPatches spec `shouldBe` []
    
    it "parses add_signature patch" $ do
      let jsonText = "{\"baseline\": \"spending\", \"patches\": [{\"op\": \"add_signature\", \"pubkey_hash\": \"#a1b2c3d4e5f6789012345678abcdef0123456789abcdef0123456789abcdef01\"}]}"
      case Json.eitherDecodeStrict (encodeUtf8 jsonText) of
        Left err -> expectationFailure $ "Parse error: " <> err
        Right (spec :: ScriptContextSpec) -> do
          scsBaseline spec `shouldBe` SpendingBaseline
          length (scsPatches spec) `shouldBe` 1
          case head (scsPatches spec) of
            AddSignature hash -> hash `shouldBe` "#a1b2c3d4e5f6789012345678abcdef0123456789abcdef0123456789abcdef01"
            _ -> expectationFailure "Expected AddSignature patch"
  
  describe "ScriptContext building" $ do
    it "builds basic spending context" $ do
      let spec = ScriptContextSpec SpendingBaseline [] Nothing
      case buildScriptContext spec of
        Left err -> expectationFailure $ "Build error: " <> show err
        Right _builtinData -> pass -- Success if it builds
    
    it "builds spending context with signature" $ do
      let patch = AddSignature "#a1b2c3d4e5f6789012345678abcdef0123456789abcdef0123456789abcdef01"
          spec = ScriptContextSpec SpendingBaseline [patch] Nothing
      case buildScriptContext spec of
        Left err -> expectationFailure $ "Build error: " <> show err
        Right _builtinData -> pass -- Success if it builds
  
  describe "Data structure references" $ do
    it "resolves simple string reference" $ do
      let dataStructures = Just $ Map.fromList [("buyer_key", Json.String "#abcdef")]
          patch = AddSignature "@buyer_key"
      case resolvePatchReferences dataStructures patch of
        Left err -> expectationFailure $ "Reference resolution error: " <> show err
        Right (AddSignature resolved) -> resolved `shouldBe` "#abcdef"
        Right _ -> expectationFailure "Expected AddSignature after resolution"
    
    it "handles missing reference with error" $ do
      let dataStructures = Just $ Map.fromList []
          patch = AddSignature "@missing_key"
      case resolvePatchReferences dataStructures patch of
        Left err -> err `shouldContain` "reference not found"
        Right _ -> expectationFailure "Expected error for missing reference"