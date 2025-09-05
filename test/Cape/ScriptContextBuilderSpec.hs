module Cape.ScriptContextBuilderSpec (spec) where

import Prelude

import Cape.ScriptContextBuilder
import PlutusCore.Data.Compact.Parser (parseBuiltinDataText)
import PlutusCore.Data.Compact.Printer (dataToCompactText)
import PlutusLedgerApi.Data.V3
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Data.AssocMap qualified as Map
import PlutusTx.Data.List qualified as List
import Test.Hspec

spec :: Spec
spec = do
  describe "createBaseline" do
    it "creates spending baseline with minimal TxInfo" do
      let baseline = createBaseline SpendingBaseline
      case scriptContextScriptInfo baseline of
        SpendingScript txOutRef maybeDatum -> do
          txOutRef `shouldSatisfy` const True
          maybeDatum `shouldBe` Just (Datum (toBuiltinData ()))
        _ -> expectationFailure "Expected SpendingScript"

    it "creates baseline with empty signatories" do
      let baseline = createBaseline SpendingBaseline
      txInfoSignatories (scriptContextTxInfo baseline) `shouldBe` List.nil

    it "creates baseline with always valid range" do
      let baseline = createBaseline SpendingBaseline
      txInfoValidRange (scriptContextTxInfo baseline) `shouldBe` always

    it "creates baseline with unit redeemer" do
      let baseline = createBaseline SpendingBaseline
      scriptContextRedeemer baseline `shouldBe` Redeemer (toBuiltinData ())

    it "creates baseline with empty inputs" do
      let baseline = createBaseline SpendingBaseline
          txInfo = scriptContextTxInfo baseline
      txInfoInputs txInfo `shouldBe` List.nil
      txInfoReferenceInputs txInfo `shouldBe` List.nil
      txInfoOutputs txInfo `shouldBe` List.nil

    it "creates baseline with zero fee" do
      let baseline = createBaseline SpendingBaseline
      txInfoFee (scriptContextTxInfo baseline) `shouldBe` Lovelace 0

    it "creates baseline with empty maps" do
      let baseline = createBaseline SpendingBaseline
          txInfo = scriptContextTxInfo baseline
      Map.null (txInfoWdrl txInfo) `shouldBe` True
      Map.null (txInfoRedeemers txInfo) `shouldBe` True
      Map.null (txInfoData txInfo) `shouldBe` True
      Map.null (txInfoVotes txInfo) `shouldBe` True

  describe "applyPatch" do
    context "AddSignature" do
      it "adds pubkey hash to signatories" do
        let baseline = createBaseline SpendingBaseline
            pubKeyHash = PubKeyHash "deadbeef"
            result = applyPatch baseline (AddSignature pubKeyHash)
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx)
              `shouldBe` List.singleton pubKeyHash
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "adds multiple signatures cumulatively" do
        let baseline = createBaseline SpendingBaseline
            pkh1 = PubKeyHash "cafe0001"
            pkh2 = PubKeyHash "cafe0002"
            patches = [AddSignature pkh1, AddSignature pkh2]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx)
              `shouldBe` List.fromSOP [pkh2, pkh1] -- Reverse order due to prepending
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "RemoveSignature" do
      it "removes signature from single-signature list" do
        let baseline = createBaseline SpendingBaseline
            pubKeyHash = PubKeyHash "deadbeef"
            patches = [AddSignature pubKeyHash, RemoveSignature pubKeyHash]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.nil
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "removes signature from multi-signature list preserving order" do
        let baseline = createBaseline SpendingBaseline
            pkh1 = PubKeyHash "cafe0001"
            pkh2 = PubKeyHash "cafe0002"
            pkh3 = PubKeyHash "cafe0003"
            patches =
              [ AddSignature pkh1
              , AddSignature pkh2
              , AddSignature pkh3
              , RemoveSignature pkh2
              ]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx)
              `shouldBe` List.fromSOP [pkh3, pkh1] -- pkh2 removed, others preserve order
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "removes signature from beginning of list" do
        let baseline = createBaseline SpendingBaseline
            pkh1 = PubKeyHash "dead0001"
            pkh2 = PubKeyHash "dead0002"
            pkh3 = PubKeyHash "dead0003"
            patches =
              [ AddSignature pkh1
              , AddSignature pkh2
              , AddSignature pkh3
              , RemoveSignature pkh3
              ]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx)
              `shouldBe` List.fromSOP [pkh2, pkh1] -- First element (pkh3) removed
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "removes signature from end of list" do
        let baseline = createBaseline SpendingBaseline
            pkh1 = PubKeyHash "beef0001"
            pkh2 = PubKeyHash "beef0002"
            pkh3 = PubKeyHash "beef0003"
            patches =
              [ AddSignature pkh1
              , AddSignature pkh2
              , AddSignature pkh3
              , RemoveSignature pkh1
              ]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx)
              `shouldBe` List.fromSOP [pkh3, pkh2] -- Last element (pkh1) removed
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "handles removing non-existent signature gracefully" do
        let baseline = createBaseline SpendingBaseline
            pkh1 = PubKeyHash "exist0001"
            pkh2 = PubKeyHash "noexist01"
            patches = [AddSignature pkh1, RemoveSignature pkh2]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx)
              `shouldBe` List.singleton pkh1 -- pkh1 remains, pkh2 removal is no-op
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "handles removing from empty signatories list gracefully" do
        let baseline = createBaseline SpendingBaseline
            pubKeyHash = PubKeyHash "nonexistent"
            result = applyPatch baseline (RemoveSignature pubKeyHash)
        case result of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.nil
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "removes all instances of duplicate signatures" do
        let baseline = createBaseline SpendingBaseline
            pkh1 = PubKeyHash "duplicate1"
            pkh2 = PubKeyHash "unique0002"
            -- Add same signature twice
            patches =
              [ AddSignature pkh1
              , AddSignature pkh2
              , AddSignature pkh1
              , RemoveSignature pkh1
              ]
        case applyPatches patches baseline of
          Right ctx ->
            txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.singleton pkh2
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "SetRedeemer" do
      it "updates the redeemer value" do
        let baseline = createBaseline SpendingBaseline
            newRedeemer = Redeemer (toBuiltinData (42 :: Integer))
            result = applyPatch baseline (SetRedeemer newRedeemer)
        case result of
          Right ctx ->
            scriptContextRedeemer ctx `shouldBe` newRedeemer
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "overwrites previous redeemer" do
        let baseline = createBaseline SpendingBaseline
            redeemer1 = Redeemer (toBuiltinData (1 :: Integer))
            redeemer2 = Redeemer (toBuiltinData (2 :: Integer))
            patches = [SetRedeemer redeemer1, SetRedeemer redeemer2]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            scriptContextRedeemer ctx `shouldBe` redeemer2
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "AddInputUTXO" do
      it
        "adds input to transaction without affecting script info when is_own_input=false"
        do
          let baseline = createBaseline SpendingBaseline
              txId = TxId "3333333333333333333333333333333333333333333333333333333333333333"
              txOutRef = TxOutRef txId 5
              value = singleton adaSymbol adaToken 50000000
              result = applyPatch baseline (AddInputUTXO txOutRef value False)
          case result of
            Right ctx -> do
              -- Should add input to transaction
              let inputs = txInfoInputs (scriptContextTxInfo ctx)
              List.length inputs `shouldBe` 1
              -- Should not change script info
              case scriptContextScriptInfo ctx of
                SpendingScript originalTxOutRef _ ->
                  originalTxOutRef `shouldNotBe` txOutRef
                _ -> expectationFailure "Expected SpendingScript"
            Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "updates spending script UTXO reference when is_own_input=true" do
        let baseline = createBaseline SpendingBaseline
            txId = TxId "3333333333333333333333333333333333333333333333333333333333333333"
            txOutRef = TxOutRef txId 5
            value = singleton adaSymbol adaToken 75000000
            result = applyPatch baseline (AddInputUTXO txOutRef value True)
        case result of
          Right ctx -> do
            -- Should add input to transaction
            let inputs = txInfoInputs (scriptContextTxInfo ctx)
            List.length inputs `shouldBe` 1
            -- Should update script info
            case scriptContextScriptInfo ctx of
              SpendingScript scriptTxOutRef _ ->
                scriptTxOutRef `shouldBe` txOutRef
              _ -> expectationFailure "Expected SpendingScript"
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "preserves datum when updating script UTXO" do
        let baseline = createBaseline SpendingBaseline
            txId = TxId "4444444444444444444444444444444444444444444444444444444444444444"
            txOutRef = TxOutRef txId 2
            value = singleton adaSymbol adaToken 100000000
            result = applyPatch baseline (AddInputUTXO txOutRef value True)
        case result of
          Right ctx -> case scriptContextScriptInfo ctx of
            SpendingScript _ maybeDatum ->
              maybeDatum `shouldBe` Just (Datum (toBuiltinData ()))
            _ -> expectationFailure "Expected SpendingScript"
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "adds multiple inputs correctly" do
        let baseline = createBaseline SpendingBaseline
            txId1 = TxId "1111111111111111111111111111111111111111111111111111111111111111"
            txId2 = TxId "2222222222222222222222222222222222222222222222222222222222222222"
            txOutRef1 = TxOutRef txId1 0
            txOutRef2 = TxOutRef txId2 1
            value1 = singleton adaSymbol adaToken 25000000
            value2 = singleton adaSymbol adaToken 50000000
            patches =
              [ AddInputUTXO txOutRef1 value1 False
              , AddInputUTXO txOutRef2 value2 True
              ]
            result = applyPatches patches baseline
        case result of
          Right ctx -> do
            -- Should have both inputs
            let inputs = txInfoInputs (scriptContextTxInfo ctx)
            List.length inputs `shouldBe` 2
            -- Script info should point to second UTXO (is_own_input=true)
            case scriptContextScriptInfo ctx of
              SpendingScript scriptTxOutRef _ ->
                scriptTxOutRef `shouldBe` txOutRef2
              _ -> expectationFailure "Expected SpendingScript"
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "SetValidRange" do
      it "sets validity range with both bounds" do
        let baseline = createBaseline SpendingBaseline
            fromTime = POSIXTime 1000
            toTime = POSIXTime 2000
            result = applyPatch baseline (SetValidRange (Just fromTime) (Just toTime))
        case result of
          Right ctx -> do
            let range = txInfoValidRange (scriptContextTxInfo ctx)
            range
              `shouldBe` Interval
                (LowerBound (Finite fromTime) True)
                (UpperBound (Finite toTime) True)
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "sets validity range with only lower bound" do
        let baseline = createBaseline SpendingBaseline
            fromTime = POSIXTime 500
            result = applyPatch baseline (SetValidRange (Just fromTime) Nothing)
        case result of
          Right ctx -> do
            let range = txInfoValidRange (scriptContextTxInfo ctx)
            range
              `shouldBe` Interval
                (LowerBound (Finite fromTime) True)
                (UpperBound PosInf True)
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "sets validity range with only upper bound" do
        let baseline = createBaseline SpendingBaseline
            toTime = POSIXTime 3000
            result = applyPatch baseline (SetValidRange Nothing (Just toTime))
        case result of
          Right ctx -> do
            let range = txInfoValidRange (scriptContextTxInfo ctx)
            range
              `shouldBe` Interval
                (LowerBound NegInf True)
                (UpperBound (Finite toTime) True)
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "sets unbounded range when both are Nothing" do
        let baseline = createBaseline SpendingBaseline
            result = applyPatch baseline (SetValidRange Nothing Nothing)
        case result of
          Right ctx -> do
            let range = txInfoValidRange (scriptContextTxInfo ctx)
            range `shouldBe` always
          Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "applyPatches" do
    it "applies multiple patches in sequence" do
      let baseline = createBaseline SpendingBaseline
          pkh = PubKeyHash "cafe1234"
          redeemer = Redeemer (toBuiltinData (99 :: Integer))
          fromTime = POSIXTime 100
          toTime = POSIXTime 200
          patches =
            [ AddSignature pkh
            , SetRedeemer redeemer
            , SetValidRange (Just fromTime) (Just toTime)
            ]
          result = applyPatches patches baseline
      case result of
        Right ctx -> do
          txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.singleton pkh
          scriptContextRedeemer ctx `shouldBe` redeemer
          txInfoValidRange (scriptContextTxInfo ctx)
            `shouldBe` Interval
              (LowerBound (Finite fromTime) True)
              (UpperBound (Finite toTime) True)
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "returns original context when no patches" do
      let baseline = createBaseline SpendingBaseline
          result = applyPatches [] baseline
      case result of
        Right ctx -> do
          -- Check key properties remain unchanged
          txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.nil
          scriptContextRedeemer ctx `shouldBe` Redeemer (toBuiltinData ())
          txInfoValidRange (scriptContextTxInfo ctx) `shouldBe` always
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "accumulates multiple signatures" do
      let baseline = createBaseline SpendingBaseline
          pkh1 = PubKeyHash "dead0001"
          pkh2 = PubKeyHash "dead0002"
          pkh3 = PubKeyHash "dead0003"
          patches =
            [ AddSignature pkh1
            , AddSignature pkh2
            , AddSignature pkh3
            ]
          result = applyPatches patches baseline
      case result of
        Right ctx ->
          txInfoSignatories (scriptContextTxInfo ctx)
            `shouldBe` List.fromSOP [pkh3, pkh2, pkh1] -- Reverse order due to prepending
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "handles complex add/remove signature interactions" do
      let baseline = createBaseline SpendingBaseline
          pkh1 = PubKeyHash "interact01"
          pkh2 = PubKeyHash "interact02"
          pkh3 = PubKeyHash "interact03"
          patches =
            [ AddSignature pkh1
            , AddSignature pkh2
            , AddSignature pkh3
            , RemoveSignature pkh2 -- Remove middle signature
            , AddSignature pkh2 -- Add it back
            , RemoveSignature pkh1 -- Remove first signature
            ]
          result = applyPatches patches baseline
      case result of
        Right ctx ->
          txInfoSignatories (scriptContextTxInfo ctx)
            `shouldBe` List.fromSOP [pkh2, pkh3] -- Final state after all operations
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "add then immediately remove returns to baseline" do
      let baseline = createBaseline SpendingBaseline
          pkh1 = PubKeyHash "tempkey01"
          patches = [AddSignature pkh1, RemoveSignature pkh1]
          result = applyPatches patches baseline
      case result of
        Right ctx ->
          txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.nil
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "remove then add ends up with signature" do
      let baseline = createBaseline SpendingBaseline
          pkh1 = PubKeyHash "addback01"
          patches = [RemoveSignature pkh1, AddSignature pkh1] -- Remove non-existent then add
          result = applyPatches patches baseline
      case result of
        Right ctx ->
          txInfoSignatories (scriptContextTxInfo ctx)
            `shouldBe` List.singleton pkh1
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "combines signatures with other patch operations" do
      let baseline = createBaseline SpendingBaseline
          pkh1 = PubKeyHash "combo0001"
          pkh2 = PubKeyHash "combo0002"
          redeemer = Redeemer (toBuiltinData (42 :: Integer))
          fromTime = POSIXTime 1000
          toTime = POSIXTime 2000
          patches =
            [ AddSignature pkh1
            , SetRedeemer redeemer
            , AddSignature pkh2
            , SetValidRange (Just fromTime) (Just toTime)
            , RemoveSignature pkh1
            ]
          result = applyPatches patches baseline
      case result of
        Right ctx -> do
          -- Verify final signature state
          txInfoSignatories (scriptContextTxInfo ctx)
            `shouldBe` List.singleton pkh2
          -- Verify other patches were applied correctly
          scriptContextRedeemer ctx `shouldBe` redeemer
          txInfoValidRange (scriptContextTxInfo ctx)
            `shouldBe` Interval
              (LowerBound (Finite fromTime) True)
              (UpperBound (Finite toTime) True)
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "buildScriptContext" do
    it "builds spending context with no patches" do
      let builder = ScriptContextBuilder SpendingBaseline []
          result = buildScriptContext builder
      case result of
        Right ctx -> do
          case scriptContextScriptInfo ctx of
            SpendingScript _ _ -> pure ()
            _ -> expectationFailure "Expected SpendingScript"
          txInfoSignatories (scriptContextTxInfo ctx) `shouldBe` List.nil
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "builds spending context with patches" do
      let pkh1 = PubKeyHash "beef0001"
          pkh2 = PubKeyHash "beef0002"
          redeemer = Redeemer (toBuiltinData (123 :: Integer))
          builder =
            ScriptContextBuilder
              SpendingBaseline
              [ AddSignature pkh1
              , AddSignature pkh2
              , SetRedeemer redeemer
              ]
          result = buildScriptContext builder
      case result of
        Right ctx -> do
          txInfoSignatories (scriptContextTxInfo ctx)
            `shouldBe` List.fromSOP [pkh2, pkh1]
          scriptContextRedeemer ctx `shouldBe` redeemer
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "builds complex spending context" do
      let pkh = PubKeyHash "complex1"
          redeemer = Redeemer (toBuiltinData ([1, 2, 3] :: [Integer]))
          txId = TxId "5555555555555555555555555555555555555555555555555555555555555555"
          txOutRef = TxOutRef txId 7
          fromTime = POSIXTime 5000
          toTime = POSIXTime 10000
          builder =
            ScriptContextBuilder
              SpendingBaseline
              [ AddSignature pkh
              , SetRedeemer redeemer
              , AddInputUTXO txOutRef (singleton adaSymbol adaToken 75000000) True
              , SetValidRange (Just fromTime) (Just toTime)
              ]
          result = buildScriptContext builder
      case result of
        Right ctx -> do
          -- Verify all patches were applied
          txInfoSignatories (scriptContextTxInfo ctx)
            `shouldBe` List.singleton pkh
          scriptContextRedeemer ctx `shouldBe` redeemer
          case scriptContextScriptInfo ctx of
            SpendingScript actualRef _ ->
              actualRef `shouldBe` txOutRef
            _ -> expectationFailure "Expected SpendingScript"
          txInfoValidRange (scriptContextTxInfo ctx)
            `shouldBe` Interval
              (LowerBound (Finite fromTime) True)
              (UpperBound (Finite toTime) True)
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "BuildError cases" do
    it "shows error details for PatchApplicationError" do
      let err =
            PatchApplicationError
              "Invalid operation"
              (AddSignature (PubKeyHash "test"))
      show err `shouldContain` "Invalid operation"
      show err `shouldContain` "AddSignature"

    it "shows error details for InvalidBaseline" do
      let err = InvalidBaseline "Unknown baseline type"
      show err `shouldContain` "Unknown baseline type"

    it "shows error details for IncompatiblePatches" do
      let err = IncompatiblePatches "Cannot apply both patches"
      show err `shouldContain` "Cannot apply both patches"

  describe "ScriptContext Data Roundtrip" do
    describe "Integer redeemer roundtrip" do
      it "roundtrips redeemer 0 through BuiltinData" do
        let redeemerValue = 0 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            builder = ScriptContextBuilder SpendingBaseline [SetRedeemer redeemer]

        case buildScriptContext builder of
          Right originalContext -> do
            -- Convert to BuiltinData and back through compact text
            let builtinData = toBuiltinData originalContext
                coreData = Builtins.builtinDataToData builtinData
                compactText = dataToCompactText coreData

            -- Parse back from text
            case parseBuiltinDataText compactText of
              Right parsedData -> do
                case fromBuiltinData (BuiltinData parsedData) of
                  Just (parsedContext :: ScriptContext) -> do
                    -- Verify redeemer extraction works correctly
                    let Redeemer redeemerBuiltinData = scriptContextRedeemer parsedContext
                    case fromBuiltinData redeemerBuiltinData of
                      Just (extractedRedeemer :: Integer) ->
                        extractedRedeemer `shouldBe` redeemerValue
                      Nothing -> expectationFailure "Failed to extract redeemer from parsed ScriptContext"
                  Nothing -> expectationFailure "Failed to parse ScriptContext from BuiltinData"
              Left parseErr -> expectationFailure $ "Failed to parse compact text: " <> show parseErr
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err

      it "roundtrips redeemer 1 through BuiltinData" do
        let redeemerValue = 1 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            builder = ScriptContextBuilder SpendingBaseline [SetRedeemer redeemer]

        case buildScriptContext builder of
          Right originalContext -> do
            let builtinData = toBuiltinData originalContext
                coreData = Builtins.builtinDataToData builtinData
                compactText = dataToCompactText coreData

            case parseBuiltinDataText compactText of
              Right parsedData -> do
                case fromBuiltinData (BuiltinData parsedData) of
                  Just (parsedContext :: ScriptContext) -> do
                    let Redeemer redeemerBuiltinData = scriptContextRedeemer parsedContext
                    case fromBuiltinData redeemerBuiltinData of
                      Just (extractedRedeemer :: Integer) ->
                        extractedRedeemer `shouldBe` redeemerValue
                      Nothing -> expectationFailure "Failed to extract redeemer from parsed ScriptContext"
                  Nothing -> expectationFailure "Failed to parse ScriptContext from BuiltinData"
              Left parseErr -> expectationFailure $ "Failed to parse compact text: " <> show parseErr
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err

      it "roundtrips redeemer 2 through BuiltinData" do
        let redeemerValue = 2 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            builder = ScriptContextBuilder SpendingBaseline [SetRedeemer redeemer]

        case buildScriptContext builder of
          Right originalContext -> do
            let builtinData = toBuiltinData originalContext
                coreData = Builtins.builtinDataToData builtinData
                compactText = dataToCompactText coreData

            case parseBuiltinDataText compactText of
              Right parsedData -> do
                case fromBuiltinData (BuiltinData parsedData) of
                  Just (parsedContext :: ScriptContext) -> do
                    let Redeemer redeemerBuiltinData = scriptContextRedeemer parsedContext
                    case fromBuiltinData redeemerBuiltinData of
                      Just (extractedRedeemer :: Integer) ->
                        extractedRedeemer `shouldBe` redeemerValue
                      Nothing -> expectationFailure "Failed to extract redeemer from parsed ScriptContext"
                  Nothing -> expectationFailure "Failed to parse ScriptContext from BuiltinData"
              Left parseErr -> expectationFailure $ "Failed to parse compact text: " <> show parseErr
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err

    describe "Complete ScriptContext structure preservation" do
      it "preserves signatures and redeemer through roundtrip" do
        let redeemerValue = 0 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            pubKeyHash =
              PubKeyHash "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            builder =
              ScriptContextBuilder
                SpendingBaseline
                [SetRedeemer redeemer, AddSignature pubKeyHash]

        case buildScriptContext builder of
          Right originalContext -> do
            let builtinData = toBuiltinData originalContext
                coreData = Builtins.builtinDataToData builtinData
                compactText = dataToCompactText coreData

            case parseBuiltinDataText compactText of
              Right parsedData -> do
                case fromBuiltinData (BuiltinData parsedData) of
                  Just (parsedContext :: ScriptContext) -> do
                    -- Verify redeemer
                    let Redeemer redeemerBuiltinData = scriptContextRedeemer parsedContext
                    case fromBuiltinData redeemerBuiltinData of
                      Just (extractedRedeemer :: Integer) ->
                        extractedRedeemer `shouldBe` redeemerValue
                      Nothing -> expectationFailure "Failed to extract redeemer from parsed ScriptContext"

                    -- Verify signatures
                    let signatures = txInfoSignatories (scriptContextTxInfo parsedContext)
                    signatures `shouldBe` List.singleton pubKeyHash
                  Nothing -> expectationFailure "Failed to parse ScriptContext from BuiltinData"
              Left parseErr -> expectationFailure $ "Failed to parse compact text: " <> show parseErr
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err

    describe "Debugging redeemer extraction logic" do
      it "matches two-party escrow validator redeemer extraction pattern" do
        let redeemerValue = 0 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            builder = ScriptContextBuilder SpendingBaseline [SetRedeemer redeemer]

        case buildScriptContext builder of
          Right originalContext -> do
            -- Test the exact redeemer extraction pattern used in two-party escrow validator
            let Redeemer redeemerBuiltinData = scriptContextRedeemer originalContext
                extractedRedeemer = fromBuiltinData redeemerBuiltinData :: Maybe Integer

            case extractedRedeemer of
              Just actualValue -> actualValue `shouldBe` redeemerValue
              Nothing -> expectationFailure "Failed to extract redeemer using validator pattern"
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err

      it "shows debug info for redeemer structure" do
        let redeemerValue = 1 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            builder = ScriptContextBuilder SpendingBaseline [SetRedeemer redeemer]

        case buildScriptContext builder of
          Right originalContext -> do
            let Redeemer (BuiltinData redeemerData) = scriptContextRedeemer originalContext
                coreData = Builtins.builtinDataToData (BuiltinData redeemerData)
                compactText = dataToCompactText coreData

            -- This should show us exactly what the redeemer looks like
            putStrLn $ "DEBUG: Redeemer data structure: " <> toString compactText
            putStrLn $ "DEBUG: Raw redeemer BuiltinData: " <> show redeemerData

            -- Verify it matches expected value
            case fromBuiltinData (BuiltinData redeemerData) of
              Just (extractedValue :: Integer) -> extractedValue `shouldBe` redeemerValue
              Nothing -> expectationFailure "Failed to extract redeemer value"
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err

      it "mimics exact measurement workflow for redeemer 0" do
        -- This test mimics the exact workflow used in the measure tool
        let redeemerValue = 0 :: Integer
            redeemer = Redeemer (toBuiltinData redeemerValue)
            pubKeyHash =
              PubKeyHash "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            builder =
              ScriptContextBuilder
                SpendingBaseline
                [SetRedeemer redeemer, AddSignature pubKeyHash]

        case buildScriptContext builder of
          Right originalContext -> do
            -- Follow the exact steps from resolveScriptContextInput
            let builtinData = toBuiltinData originalContext
                coreData = Builtins.builtinDataToData builtinData
                compactText = dataToCompactText coreData

            putStrLn $ "DEBUG: Complete ScriptContext compact: " <> toString compactText

            -- Parse back exactly like parseBuiltinDataFromText does
            case parseBuiltinDataText compactText of
              Right parsedData -> do
                let finalBuiltinData = BuiltinData parsedData

                putStrLn $ "DEBUG: Parsed back to BuiltinData: " <> show parsedData

                -- Verify we can extract the ScriptContext and redeemer
                case fromBuiltinData finalBuiltinData of
                  Just (parsedContext :: ScriptContext) -> do
                    let Redeemer redeemerBuiltinData = scriptContextRedeemer parsedContext
                    case fromBuiltinData redeemerBuiltinData of
                      Just (extractedRedeemer :: Integer) -> do
                        putStrLn $ "DEBUG: Successfully extracted redeemer: " <> show extractedRedeemer
                        extractedRedeemer `shouldBe` redeemerValue
                      Nothing -> expectationFailure "Failed to extract redeemer from final ScriptContext"
                  Nothing -> expectationFailure "Failed to parse final ScriptContext from BuiltinData"
              Left parseErr -> expectationFailure $ "Failed to parse compact text: " <> show parseErr
          Left err -> expectationFailure $ "Failed to build ScriptContext: " <> show err
