module Cape.TestsSpec (spec) where

import Prelude

import Cape.Tests
import Data.Aeson qualified as Json
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText)
import Test.Hspec

spec :: Spec
spec = describe "App.Tests" do
  describe "resolveTestInput" do
    context "script_context input type" do
      it "resolves simple ScriptContext with AddSignature patch" do
        let scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = Spending
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
        let redeemerValue = Json.Number 42
            scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = Spending
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
                { scsBaseline = Spending
                , scsPatches =
                    [ AddSignatureSpec "cafe0001"
                    , AddSignatureSpec "cafe0002"
                    , SetRedeemerSpec
                        ( Json.Array
                            ( Vector.fromList
                                [ Json.Number 1
                                , Json.Number 2
                                , Json.Number 3
                                ]
                            )
                        )
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
              Map.singleton "test_signature" (Json.String "cafe1234567890abcdef")

            scriptContextSpec =
              ScriptContextSpec
                { scsBaseline = Spending
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
                { scsBaseline = Spending
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
                { scsBaseline = Spending
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
