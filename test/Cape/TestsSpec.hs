module Cape.TestsSpec (spec) where

import Prelude

import Cape.Tests
import Data.Aeson qualified as Json
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import Test.Hspec

-- | Helper: a fully populated TestSuite with optional shared data structures.
mkSuite :: Maybe (Map Text DataStructureEntry) -> TestSuite
mkSuite ds =
  TestSuite
    { tsVersion = "2.0.0"
    , tsDescription = Just "test"
    , tsTests = []
    , tsDataStructures = ds
    }

-- | Helper: a TestInput for the given input type with no value/file/context.
emptyInput :: InputType -> TestInput
emptyInput t =
  TestInput
    { tiType = t
    , tiValue = Nothing
    , tiFile = Nothing
    , tiScriptContext = Nothing
    }

shouldResolveBuiltinData :: ResolvedInput -> Expectation
shouldResolveBuiltinData = \case
  ResolvedBuiltinData _ -> pass
  ResolvedUplc t ->
    expectationFailure $ "expected ResolvedBuiltinData, got ResolvedUplc " <> show t

spec :: Spec
spec = do
  describe "resolveTestInput" do
    context "script_context input type" do
      it "resolves simple ScriptContext with AddSignature patch" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [AddSignatureSpec "deadbeef"]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with SetRedeemer patch (UPLC text Data)" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [SetRedeemerSpec (Json.String "I 42")]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with SetRedeemer patch (Plutus JSON object)" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches =
                            [ SetRedeemerSpec
                                (Json.object ["int" Json..= (42 :: Integer)])
                            ]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with multiple patches" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches =
                            [ AddSignatureSpec "cafe0001"
                            , AddSignatureSpec "cafe0002"
                            , SetRedeemerSpec (Json.String "List [I 1, I 2, I 3]")
                            , SetValidRangeSpec (Just 1000) (Just 2000)
                            ]
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with data_structures references" do
        let dataStructures =
              Map.singleton
                "test_signature"
                (BuiltinDataEntry (Json.String "B #cafe1234567890abcdef"))
            testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [AddSignatureSpec "@test_signature"]
                        }
                }
        result <- resolveTestInput "" (mkSuite (Just dataStructures)) testInput
        shouldResolveBuiltinData result

      it "resolves ScriptContext with data_structures references (Plutus JSON form)" do
        let dataStructures =
              Map.singleton
                "test_signature"
                ( BuiltinDataEntry
                    (Json.object ["bytes" Json..= ("cafe1234567890abcdef" :: Text)])
                )
            testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = [AddSignatureSpec "@test_signature"]
                        }
                }
        result <- resolveTestInput "" (mkSuite (Just dataStructures)) testInput
        shouldResolveBuiltinData result

      it "resolves minimal ScriptContext (no patches)" do
        let testInput =
              (emptyInput ScriptContext)
                { tiScriptContext =
                    Just
                      ScriptContextSpec
                        { scsBaseline = DirectBaseline SpendingBaseline
                        , scsPatches = []
                        }
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

    context "builtin_data input type" do
      it "decodes UPLC text Data syntax string" do
        let testInput =
              (emptyInput BuiltinData)
                { tiValue = Just (Json.String "Constr 0 [I 42]")
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "decodes Plutus JSON detailed schema object" do
        let testInput =
              (emptyInput BuiltinData)
                { tiValue =
                    Just
                      ( Json.object
                          [ "constructor" Json..= (0 :: Integer)
                          , "fields" Json..= [Json.object ["int" Json..= (42 :: Integer)]]
                          ]
                      )
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        shouldResolveBuiltinData result

      it "resolves @reference into BuiltinData" do
        let dataStructures =
              Map.singleton
                "answer"
                (BuiltinDataEntry (Json.String "I 42"))
            testInput =
              (emptyInput BuiltinData)
                { tiValue = Just (Json.String "@answer")
                }
        result <- resolveTestInput "" (mkSuite (Just dataStructures)) testInput
        shouldResolveBuiltinData result

    context "uplc input type" do
      -- UPLC inputs are *terms*: applyInputsToProgram wraps them as
      -- "(program 1.1.0 <term>)" before parsing, so the value here must not
      -- itself include a (program ...) header.
      it "passes through inline UPLC term" do
        let uplcTerm = "(con integer 42)"
            testInput =
              (emptyInput UPLC)
                { tiValue = Just (Json.String uplcTerm)
                }
        result <- resolveTestInput "" (mkSuite Nothing) testInput
        case result of
          ResolvedUplc t -> t `shouldBe` uplcTerm
          ResolvedBuiltinData _ ->
            expectationFailure "expected ResolvedUplc"

  describe "PatchOperationSpec JSON completeness" do
    it "has JSON support for all constructors (exhaustive)" do
      let testJSONRoundTrip jsonValue expectedSpec = do
            case Json.eitherDecode jsonValue of
              Left err -> expectationFailure $ "Failed to parse JSON: " <> err
              Right parsedSpec -> parsedSpec `shouldBe` expectedSpec

          testAllConstructors patchSpec = case patchSpec of
            AddSignatureSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"add_signature\", \"pubkey_hash\": \"deadbeef\"}"
                (AddSignatureSpec "deadbeef")
            RemoveSignatureSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"remove_signature\", \"pubkey_hash\": \"cafe0001\"}"
                (RemoveSignatureSpec "cafe0001")
            SetRedeemerSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"set_redeemer\", \"redeemer\": \"I 42\"}"
                (SetRedeemerSpec (Json.String "I 42"))
            AddInputUTXOSpec {} ->
              testJSONRoundTrip
                [__i|{
                  "op": "add_input_utxo",
                  "utxo_ref": "txid:0",
                  "value": {
                    "lovelace": 1000000
                  },
                  "is_own_input": true
                }|]
                (AddInputUTXOSpec "txid:0" (ValueSpec 1000000 []) True Nothing)
            SetValidRangeSpec _ _ ->
              testJSONRoundTrip
                "{\"op\": \"set_valid_range\", \"from_time\": 1000, \"to_time\": 2000}"
                (SetValidRangeSpec (Just 1000) (Just 2000))
            AddOutputUTXOSpec {} ->
              testJSONRoundTrip
                [__i|{
                  "op": "add_output_utxo",
                  "address": {
                    "type": "pubkey",
                    "pubkey_hash": "deadbeef"
                  },
                  "value": {
                    "lovelace": 500000
                  }
                }|]
                (AddOutputUTXOSpec (PubkeyAddressSpec "deadbeef") (ValueSpec 500000 []))
            RemoveOutputUTXOSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"remove_output_utxo\", \"index\": 0}"
                (RemoveOutputUTXOSpec 0)
            SetScriptDatumSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"set_script_datum\", \"datum\": \"I 42\"}"
                (SetScriptDatumSpec (Json.String "I 42"))
            AddOutputUTXOWithDatumSpec {} ->
              testJSONRoundTrip
                [__i|{
                  "op": "add_output_utxo",
                  "address": {
                    "type": "pubkey",
                    "pubkey_hash": "deadbeef"
                  },
                  "value": {
                    "lovelace": 500000
                  },
                  "datum": "I 42"
                }|]
                ( AddOutputUTXOWithDatumSpec
                    (PubkeyAddressSpec "deadbeef")
                    (ValueSpec 500000 [])
                    (Json.String "I 42")
                )

      mapM_
        testAllConstructors
        [ AddSignatureSpec "test"
        , RemoveSignatureSpec "test"
        , SetRedeemerSpec (Json.String "I 42")
        , AddInputUTXOSpec "test:0" (ValueSpec 1000000 []) True Nothing
        , SetValidRangeSpec (Just 100) (Just 200)
        , AddOutputUTXOSpec (PubkeyAddressSpec "test") (ValueSpec 1000000 [])
        , RemoveOutputUTXOSpec 0
        , SetScriptDatumSpec (Json.String "I 42")
        , AddOutputUTXOWithDatumSpec
            (PubkeyAddressSpec "test")
            (ValueSpec 1000000 [])
            (Json.String "I 42")
        ]

    context "value with assets and datum" do
      it "parses add_input_utxo with assets and datum" do
        let expected :: PatchOperationSpec
            expected =
              AddInputUTXOSpec
                "txid:0"
                (ValueSpec 2000000 [AssetSpec "@cs" "@tn" 1000])
                True
                (Just (Json.String "I 42"))
        case Json.eitherDecode
          [__i|{
            "op": "add_input_utxo",
            "utxo_ref": "txid:0",
            "value": {
              "lovelace": 2000000,
              "assets": [{
                "currency_symbol": "@cs",
                "token_name": "@tn",
                "quantity": 1000
              }]
            },
            "is_own_input": true,
            "datum": "I 42"
          }|] of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right parsedSpec -> parsedSpec `shouldBe` expected

      it "parses add_output_utxo with assets" do
        let expected :: PatchOperationSpec
            expected =
              AddOutputUTXOSpec
                (ScriptAddressSpec "1111")
                (ValueSpec 2000000 [AssetSpec "@cs" "@tn" 500])
        case Json.eitherDecode
          [__i|{
            "op": "add_output_utxo",
            "address": {
              "type": "script",
              "script_hash": "1111"
            },
            "value": {
              "lovelace": 2000000,
              "assets": [{
                "currency_symbol": "@cs",
                "token_name": "@tn",
                "quantity": 500
              }]
            }
          }|] of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right parsedSpec -> parsedSpec `shouldBe` expected

  describe "Multiple inputs support" do
    context "JSON parsing with inputs array" do
      it "parses single input in inputs array" do
        let jsonText =
              [__i|{
              "name": "test_single",
              "inputs": [{"type": "uplc", "value": "(con integer 42)"}],
              "expected": {"type": "value", "content": "(con integer 42)"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_single"
            length (tcInputs testCase) `shouldBe` 1

      it "parses multiple UPLC inputs in inputs array" do
        let jsonText =
              [__i|{
              "name": "test_multi",
              "inputs": [
                {"type": "uplc", "value": "(con integer 48)"},
                {"type": "uplc", "value": "(con integer 18)"}
              ],
              "expected": {"type": "value", "content": "(con integer 6)"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_multi"
            length (tcInputs testCase) `shouldBe` 2
            tiType (tcInputs testCase !! 0) `shouldBe` UPLC
            tiType (tcInputs testCase !! 1) `shouldBe` UPLC

      it "parses empty inputs array for error-only tests" do
        let jsonText =
              [__i|{
              "name": "test_empty",
              "inputs": [],
              "expected": {"type": "error"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_empty"
            length (tcInputs testCase) `shouldBe` 0

      it "parses BuiltinData inputs in both UPLC text and Plutus JSON forms" do
        let jsonText =
              [__i|{
              "name": "test_builtin_multi",
              "inputs": [
                {"type": "builtin_data", "value": "I 42"},
                {"type": "builtin_data", "value": {"int": 100}}
              ],
              "expected": {"type": "value", "content": "(con integer 142)"}
            }|]
        case Json.eitherDecode jsonText of
          Left err -> expectationFailure $ "Failed to parse: " <> err
          Right (testCase :: TestCase) -> do
            tcName testCase `shouldBe` "test_builtin_multi"
            length (tcInputs testCase) `shouldBe` 2
            tiType (tcInputs testCase !! 0) `shouldBe` BuiltinData
            tiType (tcInputs testCase !! 1) `shouldBe` BuiltinData
