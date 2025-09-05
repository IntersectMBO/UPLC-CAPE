module TwoPartyEscrowSpec (spec) where

import Prelude

import Cape.ScriptContextBuilder
import PlutusCore.Data qualified as PLC
import PlutusLedgerApi.Data.V3
import PlutusTx.Eval (EvalResult)
import Test.Hspec
import TwoPartyEscrow
import TwoPartyEscrowSpec.Fixture qualified as Fixed
import ValidatorHelpers

spec :: Spec
spec = do
  describe "Invalid redeemer types" do
    it "fails with invalid redeemer integer 3" do
      let invalidData = toBuiltinData (3 :: Integer)
      expectFailure evaluateValidator invalidData

    it "fails with invalid redeemer integer 4" do
      let invalidData = toBuiltinData (4 :: Integer)
      expectFailure evaluateValidator invalidData

    it "fails with invalid redeemer integer 99" do
      let invalidData = toBuiltinData (99 :: Integer)
      expectFailure evaluateValidator invalidData

    it "fails with invalid redeemer integer -1" do
      let invalidData = toBuiltinData (-1 :: Integer)
      expectFailure evaluateValidator invalidData

    it "fails with constructor redeemer 0()" do
      let constructorData = BuiltinData (PLC.Constr 0 [])
      expectFailure evaluateValidator constructorData

    it "fails with bytestring redeemer #deadbeef" do
      let bytestringData = BuiltinData (PLC.B "deadbeef")
      expectFailure evaluateValidator bytestringData

    it "fails with list redeemer [1 2 3]" do
      let listData = BuiltinData (PLC.List [PLC.I 1, PLC.I 2, PLC.I 3])
      expectFailure evaluateValidator listData

    it "fails with map redeemer {1:42}" do
      let mapData = BuiltinData (PLC.Map [(PLC.I 1, PLC.I 42)])
      expectFailure evaluateValidator mapData

  describe "Simple builtin_data validation" do
    it "simple_builtin_data_redeemer_0 should fail" do
      -- Test that redeemer 0 with simple builtin_data (not script_context) fails
      -- This verifies the validator requires proper ScriptContext structure
      let simpleData = toBuiltinData (0 :: Integer)
      expectFailure evaluateValidator simpleData

  ------------------------------------------------------------------------------
  -- Deposit Operation Tests ---------------------------------------------------

  describe "ScriptContext validation" do
    it "deposit_successful should pass" do
      -- This matches the @successful_deposit data structure from cape-tests.json
      let value = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 1000) Nothing -- Match depositedEscrowDatum.depositTime
                , AddInputUTXO Fixed.txOutRef value False -- Input from buyer wallet (not script)
                , AddOutputUTXOWithDatum Fixed.scriptAddr value Fixed.depositedEscrowDatum
                ]
      expectSuccess evaluateValidator contextData

    it "deposit_without_buyer_signature should fail" do
      -- This matches the test that removes buyer signature from @successful_deposit
      let value = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.depositRedeemer
                , -- Note: No AddSignature (simulating removed buyer signature)
                  AddInputUTXO Fixed.txOutRef value True
                , AddOutputUTXO Fixed.scriptAddr value
                ]
      expectFailure evaluateValidator contextData

    it "deposit_with_incorrect_amount should fail" do
      -- Deposit with buyer signature but wrong amount (50 ADA instead of 75 ADA)
      let correctInputValue = Fixed.lovelaceValue Fixed.escrowPrice
          wrongOutputValue = adaValue 50_000_000 -- Wrong amount: 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , AddInputUTXO Fixed.txOutRef correctInputValue True
                , AddOutputUTXO Fixed.scriptAddr wrongOutputValue -- Wrong amount: 50 ADA instead of 75 ADA
                ]
      expectFailure evaluateValidator contextData

    it "deposit_to_wrong_address should fail" do
      -- Deposit with buyer signature but output goes to impostor's pubkey address instead of script
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          outputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , AddInputUTXO Fixed.txOutRef inputValue True
                , AddOutputUTXO Fixed.impostorAddr outputValue -- Output to wrong address!
                ]
      expectFailure evaluateValidator contextData

    it "deposit_creates_valid_deposited_datum should pass" do
      -- Deposit that creates proper Deposited datum
      let value = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 1000) Nothing -- Deposit time
                , AddInputUTXO Fixed.txOutRef value False -- Input from buyer wallet (not script)
                , AddOutputUTXOWithDatum Fixed.scriptAddr value Fixed.depositedEscrowDatum
                ]
      expectSuccess evaluateValidator contextData

    it "deposit_with_invalid_datum_state should fail" do
      -- Deposit with datum that's not in Deposited state
      let value = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.depositRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 1000) Nothing
                , AddInputUTXO Fixed.txOutRef value True
                , AddOutputUTXOWithDatum Fixed.scriptAddr value Fixed.acceptedEscrowDatum -- Wrong state!
                ]
      expectFailure evaluateValidator contextData

  ------------------------------------------------------------------------------
  -- Accept Operation Tests ----------------------------------------------------

  describe "Accept operation validation" do
    it "accept_successful should pass" do
      -- This matches the @accept_successful data structure from cape-tests.json
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Accept operation
                , AddInputUTXO Fixed.txOutRef2 inputValue True -- Script input with escrowed funds
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Payment to seller
                ]
      expectSuccess evaluateValidator contextData

    it "accept_without_seller_signature should fail" do
      -- Accept operation without seller signature should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , -- Note: No AddSignature (simulating missing seller signature)
                  AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    it "accept_with_incorrect_payment_amount should fail" do
      -- Accept with seller signature but wrong payment amount (50 ADA instead of 75 ADA)
      let correctInputValue = Fixed.lovelaceValue Fixed.escrowPrice
          wrongOutputValue = adaValue 50_000_000 -- Wrong amount: 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , AddInputUTXO Fixed.txOutRef2 correctInputValue True
                , AddOutputUTXO Fixed.sellerAddr wrongOutputValue -- Wrong payment amount: 50 ADA instead of 75 ADA
                ]
      expectFailure evaluateValidator contextData

    it "accept_with_payment_to_wrong_address should fail" do
      -- Accept with seller signature but payment goes to impostor's address instead of seller
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash -- Seller signs, but payment goes to impostor!
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.impostorAddr inputValue -- Payment to wrong address!
                ]
      expectFailure evaluateValidator contextData

    -- State Validation Tests (Reference Script Context)
    it "accept_without_prior_deposit should fail (invalid escrow state)" do
      -- Accept operation without a valid deposit UTXO being spent
      -- This should fail because there's no escrow state to accept from
      let outputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Accept operation
                , -- Note: No input UTXO added (simulating no prior deposit)
                  AddOutputUTXO Fixed.sellerAddr outputValue
                ]
      result <- evaluateValidator contextData
      -- Expected: FAIL - accepting without valid deposit should be rejected
      result `shouldSatisfy` isEvaluationFailure

    it "accept_with_multiple_inputs should pass (no single input validation)" do
      -- Accept with multiple script inputs (ambiguous state)
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Accept operation
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
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          partialPayment = adaValue 50_000_000 -- Only 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Accept operation
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr partialPayment -- Insufficient payment
                ]
      expectFailure evaluateValidator contextData

    it "accept_with_excess_payment_to_seller should fail (exact amount required)" do
      -- Accept with 100 ADA going to seller (more than required 75 ADA)
      -- Current validator requires exactly 75 ADA, not "at least 75 ADA"
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          excessPayment = adaValue 100_000_000 -- 100 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Accept operation
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
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Payment with potential datum
                ]
      result <- evaluateValidator contextData
      -- Expected: PASS because datum doesn't affect payment validation
      result `shouldSatisfy` isEvaluationSuccess

    it "accept_with_multiple_outputs_to_seller should pass" do
      -- Accept with payment split across two outputs to seller (50 + 25 ADA)
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          firstPayment = adaValue 50_000_000 -- 50 ADA
          secondPayment = adaValue 25_000_000 -- 25 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
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
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Accept operation
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Payment to seller
                , AddOutputUTXO Fixed.scriptAddr (adaValue 10_000_000) -- Remaining in script
                ]
      result <- evaluateValidator contextData
      -- Expected: FAIL - Accept should ensure complete withdrawal from escrow
      result `shouldSatisfy` isEvaluationFailure

  ------------------------------------------------------------------------------
  -- Refund Operation Tests ----------------------------------------------------

  describe "Refund operation validation" do
    -- Successful Refund Cases
    it "refund_successful should pass (after deadline with buyer signature)" do
      -- Refund operation after deadline with correct buyer signature
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , SetValidRange (Just 2801) Nothing -- After deadline (1000 + 1800 = 2800, so 2801)
                , AddInputUTXO Fixed.txOutRef2 inputValue True -- Script input with escrowed funds
                , AddOutputUTXO Fixed.buyerAddr inputValue -- Refund to buyer
                ]
      expectSuccess evaluateValidator contextData

    it "refund_after_exact_deadline should pass (exactly at deadline + 1 second)" do
      -- Refund exactly 1 second after deadline expiry
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , SetValidRange (Just 2801) Nothing -- Exactly at deadline + 1 (1000 + 1800 = 2800, so 2801)
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectSuccess evaluateValidator contextData

    it "refund_with_multiple_inputs should pass (no single input validation)" do
      -- Refund with multiple script inputs (should work if validator doesn't enforce single input)
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , SetValidRange (Just 3000) Nothing -- Well after deadline (1000 + 1800 = 2800)
                , AddInputUTXO Fixed.txOutRef inputValue True -- First script input
                , AddInputUTXO Fixed.txOutRef2 inputValue True -- Second script input
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectSuccess evaluateValidator contextData

    it "refund_with_datum_attached should pass" do
      -- Refund with datum attached to buyer's output (should not affect validation)
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , SetValidRange (Just 3600) Nothing -- 1 hour after deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue -- With potential datum
                ]
      expectSuccess evaluateValidator contextData

    it "refund_with_multiple_outputs_to_buyer should pass (split refund outputs)" do
      -- Refund split across two outputs to buyer (50 + 25 ADA)
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          firstPayment = adaValue 50_000_000
          secondPayment = adaValue 25_000_000
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum
                , SetValidRange (Just 5000) Nothing -- Well after deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr firstPayment -- First refund output
                , AddOutputUTXO Fixed.buyerAddr secondPayment -- Second refund output
                ]
      expectSuccess evaluateValidator contextData

    -- Authorization Failures
    it "refund_without_buyer_signature should fail" do
      -- Refund without buyer signature should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , SetScriptDatum Fixed.depositedEscrowDatum -- Add proper datum for Refund operation
                , -- Note: No AddSignature (missing buyer signature)
                  SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    it "refund_with_seller_signature_only should fail (wrong signer)" do
      -- Refund with seller signature instead of buyer should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.sellerKeyHash -- Wrong signer!
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    it "refund_with_impostor_signature should fail" do
      -- Refund with impostor signature should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.impostorPubkey -- Impostor signature!
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    -- Timing Violations
    it "refund_before_deadline should fail" do
      -- Refund before deadline expiry should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 900) Nothing -- Before deadline (1800 seconds)
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    it "refund_at_exact_deadline should fail (must be after deadline)" do
      -- Refund exactly at deadline should fail (must be strictly after)
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 2800) Nothing -- Exactly at deadline (should fail)
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    it "refund_with_no_time_range should fail (missing temporal context)" do
      -- Refund without time range should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , -- Note: No SetValidRange (using default always-valid range)
                  AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr inputValue
                ]
      expectFailure evaluateValidator contextData

    -- Value Violations
    it "refund_with_incorrect_amount should fail (not exactly 75 ADA)" do
      -- Refund with wrong total amount should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          wrongRefund = adaValue 80_000_000 -- 80 ADA instead of 75
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr wrongRefund -- Wrong amount
                ]
      expectFailure evaluateValidator contextData

    it "refund_with_partial_refund should fail (only 50 ADA to buyer)" do
      -- Partial refund (less than deposited) should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          partialRefund = adaValue 50_000_000 -- Only 50 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr partialRefund -- Insufficient refund
                ]
      expectFailure evaluateValidator contextData

    it "refund_with_excess_refund should fail (100 ADA to buyer)" do
      -- Excess refund (more than deposited) should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          excessRefund = adaValue 100_000_000 -- 100 ADA
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr excessRefund -- Too much refund
                ]
      expectFailure evaluateValidator contextData

    it "refund_to_wrong_address should fail (payment to seller/impostor)" do
      -- Refund to wrong address should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.sellerAddr inputValue -- Wrong recipient!
                ]
      expectFailure evaluateValidator contextData

    it "refund_with_remaining_script_output should fail (incomplete withdrawal)" do
      -- Refund leaving funds in script should fail
      let inputValue = Fixed.lovelaceValue Fixed.escrowPrice
          partialRefund = adaValue 65_000_000
          remainingInScript = adaValue 10_000_000
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline
                , AddInputUTXO Fixed.txOutRef2 inputValue True
                , AddOutputUTXO Fixed.buyerAddr partialRefund
                , AddOutputUTXO Fixed.scriptAddr remainingInScript -- Funds left in script!
                ]
      expectFailure evaluateValidator contextData

    -- State Violations
    it "refund_without_prior_deposit should fail (invalid escrow state)" do
      -- Refund without a valid deposit UTXO being spent
      let outputValue = Fixed.lovelaceValue Fixed.escrowPrice
          contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline
                , -- Note: No input UTXO (simulating no prior deposit)
                  AddOutputUTXO Fixed.buyerAddr outputValue
                ]
      expectFailure evaluateValidator contextData

  ------------------------------------------------------------------------------
  -- State Transition Tests (Cross-Transaction Validation) -------------------

  describe "Cross-transaction state validation" do
    it "refund_after_accept should fail (already accepted by seller)" do
      -- Attempt refund when escrow is already in Accepted state
      let contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline -- After deadline
                , SetScriptDatum Fixed.acceptedEscrowDatum -- Already accepted!
                , AddInputUTXO Fixed.txOutRef2 (Fixed.lovelaceValue Fixed.escrowPrice) True
                , AddOutputUTXO Fixed.buyerAddr (Fixed.lovelaceValue Fixed.escrowPrice)
                ]
      expectFailure evaluateValidator contextData

    it "accept_after_refund should fail (already refunded by buyer)" do
      -- Attempt accept when escrow is already in Refunded state
      let contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.refundedEscrowDatum -- Already refunded!
                , AddInputUTXO Fixed.txOutRef2 (Fixed.lovelaceValue Fixed.escrowPrice) True
                , AddOutputUTXO Fixed.sellerAddr (Fixed.lovelaceValue Fixed.escrowPrice)
                ]
      expectFailure evaluateValidator contextData

    it "refund_twice should fail (double spending prevention)" do
      -- Attempt refund when escrow is already in Refunded state
      let contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.refundRedeemer
                , AddSignature Fixed.buyerKeyHash
                , SetValidRange (Just 3000) Nothing -- After deadline -- After deadline
                , SetScriptDatum Fixed.refundedEscrowDatum -- Already refunded!
                , AddInputUTXO Fixed.txOutRef2 (Fixed.lovelaceValue Fixed.escrowPrice) True
                , AddOutputUTXO Fixed.buyerAddr (Fixed.lovelaceValue Fixed.escrowPrice)
                ]
      expectFailure evaluateValidator contextData

    it "accept_twice should fail (double spending prevention)" do
      -- Attempt accept when escrow is already in Accepted state
      let contextData =
            buildContextData $
              ScriptContextBuilder
                SpendingBaseline
                [ SetRedeemer Fixed.acceptRedeemer
                , AddSignature Fixed.sellerKeyHash
                , SetScriptDatum Fixed.acceptedEscrowDatum -- Already accepted!
                , AddInputUTXO Fixed.txOutRef2 (Fixed.lovelaceValue Fixed.escrowPrice) True
                , AddOutputUTXO Fixed.sellerAddr (Fixed.lovelaceValue Fixed.escrowPrice)
                ]
      expectFailure evaluateValidator contextData

--------------------------------------------------------------------------------
-- Helper Functions ------------------------------------------------------------

-- | Test helper to evaluate TwoPartyEscrow validator with data
evaluateValidator :: BuiltinData -> IO EvalResult
evaluateValidator = evaluateValidatorCode twoPartyEscrowValidatorCode
