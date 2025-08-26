module Cape.TestsSpec (spec) where

import Prelude

import Cape.Tests
import Data.Aeson qualified as Json
import Data.Map.Strict qualified as Map
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText)
import Test.Hspec

spec :: Spec
spec = do
  describe "resolveTestInput" do
    context "script_context input type" do
      it "resolves simple ScriptContext with AddSignature patch" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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
                { scsBaseline = DirectBaseline Spending
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

      it "resolves RawUPLC from value" do
        let uplcProgram = "(program 1.1.0 (con integer 42))"
            testInput =
              TestInput
                { tiType = RawUPLC
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
                { scsBaseline = DirectBaseline Spending
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
