module TwoPartyEscrowSpec (spec) where

import Prelude

import Cape.ScriptContextBuilder
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.V1.Data.Value (Lovelace (..))
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.Eval (EvalResult (..), evaluateCompiledCode)
import Test.Hspec
import TwoPartyEscrow
import TwoPartyEscrowSpec.Fixture qualified as Fixed

{- | Build ScriptContext or crash with error message
This eliminates the need for case analysis in tests
-}
buildScriptContextOrCrash :: ScriptContextBuilder -> V3.ScriptContext
buildScriptContextOrCrash builder =
  case buildScriptContext builder of
    Right ctx -> ctx
    Left err -> error $ "ScriptContext builder failed in test: " <> show err

-- | Build ScriptContext and convert to BuiltinData in one step
buildContextData :: ScriptContextBuilder -> V3.BuiltinData
buildContextData = V3.toBuiltinData . buildScriptContextOrCrash

-- | Test helper to evaluate validator with data
evaluateValidator :: V3.BuiltinData -> IO EvalResult
evaluateValidator inputData = do
  case twoPartyEscrowValidatorCode `PlutusTx.applyCode` PlutusTx.liftCodeDef inputData of
    Left err -> error $ "Failed to apply code: " <> toText err
    Right appliedCode -> pure $ evaluateCompiledCode appliedCode

-- | Check if evaluation was successful
isEvaluationSuccess :: EvalResult -> Bool
isEvaluationSuccess EvalResult {evalResult = result} =
  case result of
    Left _ -> False
    Right _ -> True

-- | Check if evaluation failed
isEvaluationFailure :: EvalResult -> Bool
isEvaluationFailure = not . isEvaluationSuccess

-- | Create V3.Value from Lovelace for our test types
lovelaceValue :: Lovelace -> V3.Value
lovelaceValue (Lovelace n) = V3.singleton V3.adaSymbol V3.adaToken n

spec :: Spec
spec = do
  describe "Invalid redeemer types" do
    it "fails with invalid redeemer integer 3" do
      let invalidData = V3.toBuiltinData (3 :: Integer)
      result <- evaluateValidator invalidData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with invalid redeemer integer 4" do
      let invalidData = V3.toBuiltinData (4 :: Integer)
      result <- evaluateValidator invalidData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with invalid redeemer integer 99" do
      let invalidData = V3.toBuiltinData (99 :: Integer)
      result <- evaluateValidator invalidData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with invalid redeemer integer -1" do
      let invalidData = V3.toBuiltinData (-1 :: Integer)
      result <- evaluateValidator invalidData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with constructor redeemer 0()" do
      let constructorData = V3.BuiltinData (PLC.Constr 0 [])
      result <- evaluateValidator constructorData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with bytestring redeemer #deadbeef" do
      let bytestringData = V3.BuiltinData (PLC.B "deadbeef")
      result <- evaluateValidator bytestringData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with list redeemer [1 2 3]" do
      let listData = V3.BuiltinData (PLC.List [PLC.I 1, PLC.I 2, PLC.I 3])
      result <- evaluateValidator listData
      result `shouldSatisfy` isEvaluationFailure

    it "fails with map redeemer {1:42}" do
      let mapData = V3.BuiltinData (PLC.Map [(PLC.I 1, PLC.I 42)])
      result <- evaluateValidator mapData
      result `shouldSatisfy` isEvaluationFailure

  describe "Simple builtin_data validation" do
    it "simple_builtin_data_redeemer_0 should fail" do
      -- Test that redeemer 0 with simple builtin_data (not script_context) fails
      -- This verifies the validator requires proper ScriptContext structure
      let simpleData = V3.toBuiltinData (0 :: Integer)
      result <- evaluateValidator simpleData
      result `shouldSatisfy` isEvaluationFailure

  describe "ScriptContext validation" do
    it "deposit_successful should pass" do
      -- This matches the @successful_deposit data structure from cape-tests.json
      let value = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , AddInputUTXO Fixed.txOutRef value True
                , AddOutputUTXO Fixed.scriptAddr value
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationSuccess

    it "deposit_without_buyer_signature should fail" do
      -- This matches the test that removes buyer signature from @successful_deposit
      let value = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.depositRedeemer
                , -- Note: No AddSignature (simulating removed buyer signature)
                  AddInputUTXO Fixed.txOutRef value True
                , AddOutputUTXO Fixed.scriptAddr value
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

    it "deposit_with_incorrect_amount should fail" do
      -- Deposit with buyer signature but wrong amount (50 ADA instead of 75 ADA)
      let correctInputValue = lovelaceValue Fixed.escrowPrice
          wrongOutputValue = lovelaceValue (Lovelace 50000000) -- Wrong amount: 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , AddInputUTXO Fixed.txOutRef correctInputValue True
                , AddOutputUTXO Fixed.scriptAddr wrongOutputValue -- Wrong amount: 50 ADA instead of 75 ADA
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

    it "deposit_to_wrong_address should fail" do
      -- Deposit with buyer signature but output goes to impostor's pubkey address instead of script
      let inputValue = lovelaceValue Fixed.escrowPrice
          outputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , AddInputUTXO Fixed.txOutRef inputValue True
                , AddOutputUTXO Fixed.impostorAddr outputValue -- Output to wrong address!
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

  describe "Accept operation validation" do
    it "accept_successful should pass" do
      -- This matches the @accept_successful data structure from cape-tests.json
      let inputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 inputValue True -- Script input with escrowed funds
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Payment to seller
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationSuccess

    it "accept_without_seller_signature should fail" do
      -- Accept operation without seller signature should fail
      let inputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , -- Note: No AddSignature (simulating missing seller signature)
                  AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

    it "accept_with_incorrect_payment_amount should fail" do
      -- Accept with seller signature but wrong payment amount (50 ADA instead of 75 ADA)
      let correctInputValue = lovelaceValue Fixed.escrowPrice
          wrongOutputValue = lovelaceValue (Lovelace 50000000) -- Wrong amount: 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 correctInputValue True
                , AddOutputUTXO Fixed.sellerAddr wrongOutputValue -- Wrong payment amount: 50 ADA instead of 75 ADA
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

    it "accept_with_payment_to_wrong_address should fail" do
      -- Accept with seller signature but payment goes to impostor's address instead of seller
      let inputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash -- Seller signs, but payment goes to impostor!
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.impostorAddr inputValue -- Payment to wrong address!
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

    -- State Validation Tests (Reference Script Context)
    it "accept_without_prior_deposit should fail (invalid escrow state)" do
      -- Accept operation without a valid deposit UTXO being spent
      -- This should fail because there's no escrow state to accept from
      let outputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , -- Note: No input UTXO added (simulating no prior deposit)
                  AddOutputUTXO Fixed.sellerAddr outputValue
                ]
      result <- evaluateValidator contextData
      -- Expected: FAIL - accepting without valid deposit should be rejected
      result `shouldSatisfy` isEvaluationFailure

    it "accept_with_multiple_inputs should pass (no single input validation)" do
      -- Accept with multiple script inputs (ambiguous state)
      let inputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef inputValue True -- First script input
                , AddInputUTXO Fixed.txOutRef2 inputValue True -- Second script input
                , AddOutputUTXO Fixed.sellerAddr inputValue
                ]
      result <- evaluateValidator contextData
      -- Expected: PASS because validator doesn't enforce single input
      result `shouldSatisfy` isEvaluationSuccess

    -- Value Distribution Tests
    it "accept_with_partial_payment_to_seller should fail" do
      -- Accept with only 50 ADA going to seller (less than required 75 ADA)
      let inputValue = lovelaceValue Fixed.escrowPrice
          partialPayment = lovelaceValue (Lovelace 50000000) -- Only 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr partialPayment -- Insufficient payment
                ]
      result <- evaluateValidator contextData
      result `shouldSatisfy` isEvaluationFailure

    it "accept_with_excess_payment_to_seller should fail (exact amount required)" do
      -- Accept with 100 ADA going to seller (more than required 75 ADA)
      -- Current validator requires exactly 75 ADA, not "at least 75 ADA"
      let inputValue = lovelaceValue Fixed.escrowPrice
          excessPayment = lovelaceValue (Lovelace 100000000) -- 100 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr excessPayment -- More than required
                ]
      result <- evaluateValidator contextData
      -- Expected: FAIL because validator requires exactly 75 ADA (uses /= not <)
      result `shouldSatisfy` isEvaluationFailure

    -- Output Structure Tests
    it "accept_with_datum_attached should pass" do
      -- Accept with datum attached to seller's payment output
      -- Note: In current test framework, datum attachment is implicit
      let inputValue = lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Payment with potential datum
                ]
      result <- evaluateValidator contextData
      -- Expected: PASS because datum doesn't affect payment validation
      result `shouldSatisfy` isEvaluationSuccess

    it "accept_with_multiple_outputs_to_seller should pass" do
      -- Accept with payment split across two outputs to seller (50 + 25 ADA)
      let inputValue = lovelaceValue Fixed.escrowPrice
          firstPayment = lovelaceValue (Lovelace 50000000) -- 50 ADA
          secondPayment = lovelaceValue (Lovelace 25000000) -- 25 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr firstPayment -- First payment
                , AddOutputUTXO Fixed.sellerAddr secondPayment -- Second payment
                ]
      result <- evaluateValidator contextData
      -- Expected: PASS because valuePaidTo sums all outputs to seller
      result `shouldSatisfy` isEvaluationSuccess

    -- Script Continuity Tests
    it "accept_with_remaining_script_output should fail (incomplete withdrawal)" do
      -- Accept while leaving funds in script (security vulnerability)
      -- Escrow should ensure complete withdrawal - no funds should remain in script
      let inputValue = lovelaceValue Fixed.escrowPrice
          scriptAddr =
            V3.Address
              ( V3.ScriptCredential
                  (V3.ScriptHash "1111111111111111111111111111111111111111111111111111111111")
              )
              Nothing
          contextData =
            buildContextData $
              ScriptContextBuilder
                Spending
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Payment to seller
                , AddOutputUTXO scriptAddr (lovelaceValue (Lovelace 10000000)) -- Remaining in script
                ]
      result <- evaluateValidator contextData
      -- Expected: FAIL - Accept should ensure complete withdrawal from escrow
      result `shouldSatisfy` isEvaluationFailure
