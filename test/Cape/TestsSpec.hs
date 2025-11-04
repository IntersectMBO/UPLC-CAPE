module Cape.TestsSpec (spec) where

import Prelude

import Cape.Tests
import Data.Aeson qualified as Json
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText)
import Test.Hspec

spec :: Spec
spec = do
  describe "resolveTestInput" do
    context "script_context input type" do
      it "resolves simple ScriptContext with AddSignature patch" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches = [AddSignatureSpec "deadbeef"]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- The result should be a valid BuiltinData text representation
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse resolved ScriptContext: " <> show parseErr
          Right _ -> pass -- Success - it's valid BuiltinData
      it "resolves ScriptContext with SetRedeemer patch" do
        let redeemerValue = Json.String "42"
            scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches = [SetRedeemerSpec redeemerValue]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- Verify it produces valid BuiltinData
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse resolved ScriptContext: " <> show parseErr
          Right _ -> pass

      it "resolves ScriptContext with multiple patches" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches =
                    [ AddSignatureSpec "cafe0001"
                    , AddSignatureSpec "cafe0002"
                    , SetRedeemerSpec (Json.String "[1 2 3]")
                    , SetValidRangeSpec (Just 1000) (Just 2000)
                    ]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- Verify it produces valid BuiltinData
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse resolved ScriptContext: " <> show parseErr
          Right _ -> pass

      it "resolves ScriptContext with data_structures references" do
        let dataStructures =
              Map.singleton
                "test_signature"
                (BuiltinDataEntry (Json.String "#cafe1234567890abcdef"))

            scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches = [AddSignatureSpec "@test_signature"]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Just dataStructures
                }

        result <- resolveTestInput "" testSuite testInput

        -- Verify it produces valid BuiltinData
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $ "Failed to parse resolved ScriptContext: " <> show parseErr
          Right _ -> pass

      it "handles ScriptContext type properly" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches = []
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- Should produce valid BuiltinData
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse ScriptContext result: " <> show parseErr
          Right _ -> pass

      it "resolves ScriptContext for Accept operation with seller signature" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches =
                    [ SetRedeemerSpec (Json.String "1") -- Accept redeemer
                    , AddSignatureSpec
                        "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" -- Seller signature
                    , AddInputUTXOSpec
                        "4444444444444444444444444444444444444444444444444444444444444444:0"
                        75000000
                        True
                    , AddOutputUTXOSpec
                        ( PubkeyAddressSpec
                            "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                        )
                        75000000
                    ]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "Accept operation test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- The result should be a valid BuiltinData text representation
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse resolved Accept ScriptContext: " <> show parseErr
          Right _ -> pass -- Success - it's valid BuiltinData for Accept
      it "resolves ScriptContext with RemoveSignature patch" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches =
                    [ AddSignatureSpec "cafe0001"
                    , AddSignatureSpec "cafe0002"
                    , RemoveSignatureSpec "cafe0001"
                    ]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- The result should be a valid BuiltinData text representation
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse resolved ScriptContext: " <> show parseErr
          Right _ -> pass -- Success - it's valid BuiltinData
      it "resolves ScriptContext with RemoveSignature and @references" do
        let dataStructures =
              Map.fromList
                [ ("test_key1", BuiltinDataEntry (Json.String "#deadbeef0001"))
                , ("test_key2", BuiltinDataEntry (Json.String "#deadbeef0002"))
                ]

            scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches =
                    [ AddSignatureSpec "@test_key1"
                    , AddSignatureSpec "@test_key2"
                    , RemoveSignatureSpec "@test_key1"
                    ]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Just dataStructures
                }

        result <- resolveTestInput "" testSuite testInput

        -- Verify it produces valid BuiltinData
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $ "Failed to parse resolved ScriptContext: " <> show parseErr
          Right _ -> pass

      it "handles RemoveSignature with mixed literal and reference pubkeys" do
        let dataStructures =
              Map.singleton
                "ref_key"
                (BuiltinDataEntry (Json.String "#deadbeef123456"))

            scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches =
                    [ AddSignatureSpec "literal0001"
                    , AddSignatureSpec "@ref_key"
                    , AddSignatureSpec "literal0002"
                    , RemoveSignatureSpec "literal0001"
                    , RemoveSignatureSpec "@ref_key"
                    ]
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Just dataStructures
                }

        result <- resolveTestInput "" testSuite testInput

        -- Should produce valid BuiltinData with only literal0002 signature remaining
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse ScriptContext result: " <> show parseErr
          Right _ -> pass

    context "other input types" do
      it "resolves BuiltinData from value" do
        let testInput =
              TestInput
                { tiType = BuiltinData
                , tiValue = Just "Constr 0 [I 42]"
                , tiFile = Nothing
                , tiScriptContext = Nothing
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput
        result `shouldBe` "Constr 0 [I 42]"

      it "resolves UPLC from value" do
        let uplcProgram = "(program 1.1.0 (con integer 42))"
            testInput =
              TestInput
                { tiType = UPLC
                , tiValue = Just uplcProgram
                , tiFile = Nothing
                , tiScriptContext = Nothing
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput
        result `shouldBe` uplcProgram

    context "integration scenarios" do
      it "works with empty patches (minimal ScriptContext)" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline SpendingBaseline
                , scsPatches = []
                }
            testInput =
              TestInput
                { tiType = ScriptContext
                , tiValue = Nothing
                , tiFile = Nothing
                , tiScriptContext = Just scriptContextSpec
                }
            testSuite =
              TestSuite
                { tsVersion = "1.0.0"
                , tsDescription = Just "test"
                , tsTests = []
                , tsDataStructures = Nothing
                }

        result <- resolveTestInput "" testSuite testInput

        -- Verify it produces valid BuiltinData for minimal ScriptContext
        case parseBuiltinDataText result of
          Left parseErr ->
            expectationFailure $
              "Failed to parse minimal ScriptContext: " <> show parseErr
          Right _ -> pass

  describe "PatchOperationSpec JSON completeness" do
    it "has JSON support for all constructors (exhaustive)" do
      -- This test uses exhaustive pattern matching without wildcards.
      -- Adding new PatchOperationSpec constructors will break compilation,
      -- forcing developers to add corresponding JSON test cases.
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
                "{\"op\": \"set_redeemer\", \"redeemer\": \"42\"}"
                (SetRedeemerSpec (Json.String "42"))
            AddInputUTXOSpec _ _ _ ->
              testJSONRoundTrip
                "{\"op\": \"add_input_utxo\", \"utxo_ref\": \"txid:0\", \"lovelace\": 1000000, \"is_own_input\": true}"
                (AddInputUTXOSpec "txid:0" 1000000 True)
            SetValidRangeSpec _ _ ->
              testJSONRoundTrip
                "{\"op\": \"set_valid_range\", \"from_time\": 1000, \"to_time\": 2000}"
                (SetValidRangeSpec (Just 1000) (Just 2000))
            AddOutputUTXOSpec _ _ ->
              testJSONRoundTrip
                "{\"op\": \"add_output_utxo\", \"address\": {\"type\": \"pubkey\", \"pubkey_hash\": \"deadbeef\"}, \"lovelace\": 500000}"
                (AddOutputUTXOSpec (PubkeyAddressSpec "deadbeef") 500000)
            RemoveOutputUTXOSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"remove_output_utxo\", \"index\": 0}"
                (RemoveOutputUTXOSpec 0)
            SetScriptDatumSpec _ ->
              testJSONRoundTrip
                "{\"op\": \"set_script_datum\", \"datum\": \"42\"}"
                (SetScriptDatumSpec (Json.String "42"))
            AddOutputUTXOWithDatumSpec _ _ _ ->
              testJSONRoundTrip
                "{\"op\": \"add_output_utxo\", \"address\": {\"type\": \"pubkey\", \"pubkey_hash\": \"deadbeef\"}, \"lovelace\": 500000, \"datum\": \"42\"}"
                ( AddOutputUTXOWithDatumSpec
                    (PubkeyAddressSpec "deadbeef")
                    500000
                    (Json.String "42")
                )
      -- NO wildcard pattern! Compilation will fail if a constructor is added

      -- Test with one instance of each constructor
      mapM_
        testAllConstructors
        [ AddSignatureSpec "test"
        , RemoveSignatureSpec "test"
        , SetRedeemerSpec (Json.String "test")
        , AddInputUTXOSpec "test:0" 1000000 True
        , SetValidRangeSpec (Just 100) (Just 200)
        , AddOutputUTXOSpec (PubkeyAddressSpec "test") 1000000
        , RemoveOutputUTXOSpec 0
        , SetScriptDatumSpec (Json.String "test")
        , AddOutputUTXOWithDatumSpec
            (PubkeyAddressSpec "test")
            1000000
            (Json.String "test")
        ]

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

      it "parses multiple BuiltinData inputs" do
        let jsonText =
              [__i|{
              "name": "test_builtin_multi",
              "inputs": [
                {"type": "builtin_data", "value": "42"},
                {"type": "builtin_data", "value": "100"}
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
