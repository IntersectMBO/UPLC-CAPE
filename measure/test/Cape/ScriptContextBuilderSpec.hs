module Cape.ScriptContextBuilderSpec (spec) where

import Prelude

import Cape.ScriptContextBuilder
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.AssocMap qualified as Map
import Test.Hspec

spec :: Spec
spec = describe "ScriptContextBuilder" do
  describe "createBaseline" do
    it "creates spending baseline with minimal TxInfo" do
      let baseline = createBaseline Spending
      case V3.scriptContextScriptInfo baseline of
        V3.SpendingScript txOutRef maybeDatum -> do
          txOutRef `shouldSatisfy` const True
          maybeDatum `shouldBe` Just (V3.Datum (V3.toBuiltinData ()))
        _ -> expectationFailure "Expected SpendingScript"

    it "creates baseline with empty signatories" do
      let baseline = createBaseline Spending
      V3.txInfoSignatories (V3.scriptContextTxInfo baseline) `shouldBe` []

    it "creates baseline with always valid range" do
      let baseline = createBaseline Spending
      V3.txInfoValidRange (V3.scriptContextTxInfo baseline) `shouldBe` V3.always

    it "creates baseline with unit redeemer" do
      let baseline = createBaseline Spending
      V3.scriptContextRedeemer baseline `shouldBe` V3.Redeemer (V3.toBuiltinData ())

    it "creates baseline with empty inputs" do
      let baseline = createBaseline Spending
          txInfo = V3.scriptContextTxInfo baseline
      V3.txInfoInputs txInfo `shouldBe` []
      V3.txInfoReferenceInputs txInfo `shouldBe` []
      V3.txInfoOutputs txInfo `shouldBe` []

    it "creates baseline with zero fee" do
      let baseline = createBaseline Spending
      V3.txInfoFee (V3.scriptContextTxInfo baseline) `shouldBe` V3.Lovelace 0

    it "creates baseline with empty maps" do
      let baseline = createBaseline Spending
          txInfo = V3.scriptContextTxInfo baseline
      Map.null (V3.txInfoWdrl txInfo) `shouldBe` True
      Map.null (V3.txInfoRedeemers txInfo) `shouldBe` True
      Map.null (V3.txInfoData txInfo) `shouldBe` True
      Map.null (V3.txInfoVotes txInfo) `shouldBe` True

  describe "applyPatch" do
    context "AddSignature" do
      it "adds pubkey hash to signatories" do
        let baseline = createBaseline Spending
            pubKeyHash = V3.PubKeyHash "deadbeef"
            result = applyPatch baseline (AddSignature pubKeyHash)
        case result of
          Right ctx ->
            V3.txInfoSignatories (V3.scriptContextTxInfo ctx) `shouldBe` [pubKeyHash]
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "adds multiple signatures cumulatively" do
        let baseline = createBaseline Spending
            pkh1 = V3.PubKeyHash "cafe0001"
            pkh2 = V3.PubKeyHash "cafe0002"
            patches = [AddSignature pkh1, AddSignature pkh2]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            V3.txInfoSignatories (V3.scriptContextTxInfo ctx)
              `shouldBe` [pkh2, pkh1] -- Reverse order due to prepending
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "SetRedeemer" do
      it "updates the redeemer value" do
        let baseline = createBaseline Spending
            newRedeemer = V3.Redeemer (V3.toBuiltinData (42 :: Integer))
            result = applyPatch baseline (SetRedeemer newRedeemer)
        case result of
          Right ctx ->
            V3.scriptContextRedeemer ctx `shouldBe` newRedeemer
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "overwrites previous redeemer" do
        let baseline = createBaseline Spending
            redeemer1 = V3.Redeemer (V3.toBuiltinData (1 :: Integer))
            redeemer2 = V3.Redeemer (V3.toBuiltinData (2 :: Integer))
            patches = [SetRedeemer redeemer1, SetRedeemer redeemer2]
            result = applyPatches patches baseline
        case result of
          Right ctx ->
            V3.scriptContextRedeemer ctx `shouldBe` redeemer2
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "SetSpendingUTXO" do
      it "updates spending script UTXO reference" do
        let baseline = createBaseline Spending
            txId = V3.TxId "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
            newTxOutRef = V3.TxOutRef txId 5
            result = applyPatch baseline (SetSpendingUTXO newTxOutRef)
        case result of
          Right ctx -> case V3.scriptContextScriptInfo ctx of
            V3.SpendingScript txOutRef _ ->
              txOutRef `shouldBe` newTxOutRef
            _ -> expectationFailure "Expected SpendingScript"
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "preserves datum when updating UTXO" do
        let baseline = createBaseline Spending
            txId = V3.TxId "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
            newTxOutRef = V3.TxOutRef txId 2
            result = applyPatch baseline (SetSpendingUTXO newTxOutRef)
        case result of
          Right ctx -> case V3.scriptContextScriptInfo ctx of
            V3.SpendingScript _ maybeDatum ->
              maybeDatum `shouldBe` Just (V3.Datum (V3.toBuiltinData ()))
            _ -> expectationFailure "Expected SpendingScript"
          Left err -> expectationFailure $ "Unexpected error: " <> show err

    context "SetValidRange" do
      it "sets validity range with both bounds" do
        let baseline = createBaseline Spending
            fromTime = V3.POSIXTime 1000
            toTime = V3.POSIXTime 2000
            result = applyPatch baseline (SetValidRange (Just fromTime) (Just toTime))
        case result of
          Right ctx -> do
            let range = V3.txInfoValidRange (V3.scriptContextTxInfo ctx)
            range
              `shouldBe` V3.Interval
                (V3.LowerBound (V3.Finite fromTime) True)
                (V3.UpperBound (V3.Finite toTime) True)
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "sets validity range with only lower bound" do
        let baseline = createBaseline Spending
            fromTime = V3.POSIXTime 500
            result = applyPatch baseline (SetValidRange (Just fromTime) Nothing)
        case result of
          Right ctx -> do
            let range = V3.txInfoValidRange (V3.scriptContextTxInfo ctx)
            range
              `shouldBe` V3.Interval
                (V3.LowerBound (V3.Finite fromTime) True)
                (V3.UpperBound V3.PosInf True)
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "sets validity range with only upper bound" do
        let baseline = createBaseline Spending
            toTime = V3.POSIXTime 3000
            result = applyPatch baseline (SetValidRange Nothing (Just toTime))
        case result of
          Right ctx -> do
            let range = V3.txInfoValidRange (V3.scriptContextTxInfo ctx)
            range
              `shouldBe` V3.Interval
                (V3.LowerBound V3.NegInf True)
                (V3.UpperBound (V3.Finite toTime) True)
          Left err -> expectationFailure $ "Unexpected error: " <> show err

      it "sets unbounded range when both are Nothing" do
        let baseline = createBaseline Spending
            result = applyPatch baseline (SetValidRange Nothing Nothing)
        case result of
          Right ctx -> do
            let range = V3.txInfoValidRange (V3.scriptContextTxInfo ctx)
            range `shouldBe` V3.always
          Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "applyPatches" do
    it "applies multiple patches in sequence" do
      let baseline = createBaseline Spending
          pkh = V3.PubKeyHash "cafe1234"
          redeemer = V3.Redeemer (V3.toBuiltinData (99 :: Integer))
          fromTime = V3.POSIXTime 100
          toTime = V3.POSIXTime 200
          patches =
            [ AddSignature pkh
            , SetRedeemer redeemer
            , SetValidRange (Just fromTime) (Just toTime)
            ]
          result = applyPatches patches baseline
      case result of
        Right ctx -> do
          V3.txInfoSignatories (V3.scriptContextTxInfo ctx) `shouldBe` [pkh]
          V3.scriptContextRedeemer ctx `shouldBe` redeemer
          V3.txInfoValidRange (V3.scriptContextTxInfo ctx)
            `shouldBe` V3.Interval
              (V3.LowerBound (V3.Finite fromTime) True)
              (V3.UpperBound (V3.Finite toTime) True)
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "returns original context when no patches" do
      let baseline = createBaseline Spending
          result = applyPatches [] baseline
      case result of
        Right ctx -> do
          -- Check key properties remain unchanged
          V3.txInfoSignatories (V3.scriptContextTxInfo ctx) `shouldBe` []
          V3.scriptContextRedeemer ctx `shouldBe` V3.Redeemer (V3.toBuiltinData ())
          V3.txInfoValidRange (V3.scriptContextTxInfo ctx) `shouldBe` V3.always
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "accumulates multiple signatures" do
      let baseline = createBaseline Spending
          pkh1 = V3.PubKeyHash "dead0001"
          pkh2 = V3.PubKeyHash "dead0002"
          pkh3 = V3.PubKeyHash "dead0003"
          patches =
            [ AddSignature pkh1
            , AddSignature pkh2
            , AddSignature pkh3
            ]
          result = applyPatches patches baseline
      case result of
        Right ctx ->
          V3.txInfoSignatories (V3.scriptContextTxInfo ctx)
            `shouldBe` [pkh3, pkh2, pkh1] -- Reverse order due to prepending
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "buildScriptContext" do
    it "builds spending context with no patches" do
      let builder = ScriptContextBuilder Spending []
          result = buildScriptContext builder
      case result of
        Right ctx -> do
          case V3.scriptContextScriptInfo ctx of
            V3.SpendingScript _ _ -> pure ()
            _ -> expectationFailure "Expected SpendingScript"
          V3.txInfoSignatories (V3.scriptContextTxInfo ctx) `shouldBe` []
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "builds spending context with patches" do
      let pkh1 = V3.PubKeyHash "beef0001"
          pkh2 = V3.PubKeyHash "beef0002"
          redeemer = V3.Redeemer (V3.toBuiltinData (123 :: Integer))
          builder =
            ScriptContextBuilder
              Spending
              [ AddSignature pkh1
              , AddSignature pkh2
              , SetRedeemer redeemer
              ]
          result = buildScriptContext builder
      case result of
        Right ctx -> do
          V3.txInfoSignatories (V3.scriptContextTxInfo ctx)
            `shouldBe` [pkh2, pkh1]
          V3.scriptContextRedeemer ctx `shouldBe` redeemer
        Left err -> expectationFailure $ "Unexpected error: " <> show err

    it "builds complex spending context" do
      let pkh = V3.PubKeyHash "complex1"
          redeemer = V3.Redeemer (V3.toBuiltinData ([1, 2, 3] :: [Integer]))
          txId = V3.TxId "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
          txOutRef = V3.TxOutRef txId 7
          fromTime = V3.POSIXTime 5000
          toTime = V3.POSIXTime 10000
          builder =
            ScriptContextBuilder
              Spending
              [ AddSignature pkh
              , SetRedeemer redeemer
              , SetSpendingUTXO txOutRef
              , SetValidRange (Just fromTime) (Just toTime)
              ]
          result = buildScriptContext builder
      case result of
        Right ctx -> do
          -- Verify all patches were applied
          V3.txInfoSignatories (V3.scriptContextTxInfo ctx) `shouldBe` [pkh]
          V3.scriptContextRedeemer ctx `shouldBe` redeemer
          case V3.scriptContextScriptInfo ctx of
            V3.SpendingScript actualRef _ ->
              actualRef `shouldBe` txOutRef
            _ -> expectationFailure "Expected SpendingScript"
          V3.txInfoValidRange (V3.scriptContextTxInfo ctx)
            `shouldBe` V3.Interval
              (V3.LowerBound (V3.Finite fromTime) True)
              (V3.UpperBound (V3.Finite toTime) True)
        Left err -> expectationFailure $ "Unexpected error: " <> show err

  describe "BuildError cases" do
    it "shows error details for PatchApplicationError" do
      let err =
            PatchApplicationError "Invalid operation" (AddSignature (V3.PubKeyHash "test"))
      show err `shouldContain` "Invalid operation"
      show err `shouldContain` "AddSignature"

    it "shows error details for InvalidBaseline" do
      let err = InvalidBaseline "Unknown baseline type"
      show err `shouldContain` "Unknown baseline type"

    it "shows error details for IncompatiblePatches" do
      let err = IncompatiblePatches "Cannot apply both patches"
      show err `shouldContain` "Cannot apply both patches"
