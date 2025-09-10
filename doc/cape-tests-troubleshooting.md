# CAPE Tests Troubleshooting Guide

## Foundational Principle

**Haskell spec is the source of truth for test cases and `cape-tests.json` should be aligned with it.**

When CAPE test failures occur, the Haskell test specification (typically in files like `test/*Spec.hs`) represents the authoritative definition of expected behavior. The `cape-tests.json` configurations should be systematically aligned with these specifications to ensure consistent test execution.

## Systematic Debugging Methodology

### 1. Establish the Reference Implementation

Before investigating failures, identify the canonical test specification:

- Locate the corresponding Haskell spec file (e.g., `test/TwoPartyEscrowSpec.hs`)
- Identify the passing test cases that represent correct behavior
- Use these as your reference implementation for comparison

### 2. Compare ScriptContext Encodings

The most effective debugging approach involves comparing the ScriptContext data structures between Haskell spec and `cape-tests.json`:

**Debug Instrumentation Setup:**

Add temporary debug output to your Haskell spec file:

```haskell
-- Add these imports at the top
import PlutusCore.Data qualified as PLC
import PlutusCore.Data.Compact.Printer (dataToCompactText)
import PlutusTx.Builtins qualified as Builtins

-- Add this helper function
debugScriptContext :: String -> BuiltinData -> IO ()
debugScriptContext testName contextData = do
  let plcData = Builtins.builtinDataToData contextData
      compactText = dataToCompactText plcData
  putStrLn $ "DEBUG[" <> testName <> "]: " <> show compactText
```

**Usage in test cases:**

```haskell
it "example_test should pass" do
  let contextData = buildContextData $ ScriptContextBuilder ...
  debugScriptContext "example_test" contextData
  expectSuccess evaluateValidator contextData
```

### 3. Systematic Investigation Process

**Step A: Run Haskell Tests with Debug Output**

- Execute the Haskell test suite to capture ScriptContext encodings
- Focus on tests that correspond to failing CAPE tests
- Record the debug output for successful test cases

**Step B: Compare with CAPE Test Configuration**

- Examine the corresponding test case in `cape-tests.json`
- Look for structural differences between Haskell-generated ScriptContext and JSON configuration
- Pay attention to:
  - Script context patch operations sequence
  - UTXO references and their consistency
  - Datum attachments and script datum operations
  - Timing values and valid range configurations
  - Signature requirements and pubkey references

**Step C: Identify Systematic Patterns**

- Look for recurring discrepancies across multiple failing tests
- Common issues often include:
  - Missing or incorrect patch operations
  - Inconsistent reference values between test cases
  - Incorrect state assumptions (wrong datum states)
  - Misaligned timing or validation parameters

**Step D: Apply Systematic Fixes**

- Once patterns are identified, apply consistent fixes across all affected test cases
- Verify that fixes maintain logical consistency within the test scenario
- Re-run tests after each fix to confirm resolution

### 4. Validation and Cleanup

**Post-Fix Verification:**

```bash
# Run the full test suite to verify all tests pass
cape submission verify submissions/target-scenario/

# Check that performance metrics are consistent
cape submission measure submissions/target-scenario/
```

**Debug Cleanup:**

- Remove all temporary debug instrumentation from spec files
- Clean up any additional imports that were added for debugging
- Run code formatting to maintain consistency

## Debug Instrumentation Guidelines

### When to Add Debug Output

- When CAPE tests fail but corresponding Haskell tests pass
- When there are systematic failures across multiple related test cases
- When investigating ScriptContext structure mismatches

### Instrumentation Best Practices

- Use descriptive test names that match between Haskell spec and `cape-tests.json`
- Capture debug output for both passing and failing test cases for comparison
- Focus on the complete ScriptContext encoding rather than individual components
- Remove all debug code after troubleshooting is complete

### What to Compare

1. **Redeemer Values**: Ensure integer values match expected operations
2. **Input/Output Structure**: Verify UTXO references and values align
3. **Datum Operations**: Check that script datum operations are present when required
4. **Signature Requirements**: Confirm required signatures are included
5. **Temporal Constraints**: Validate timing values and valid range settings

## Best Practices for Maintaining Test Alignment

### Design Principles

- Keep test case names consistent between Haskell specs and JSON configurations
- Use common reference values (transaction IDs, addresses, amounts) across test suites
- Maintain clear separation between positive and negative test cases
- Document the expected behavior and reasoning for complex test scenarios

### Preventive Measures

- Regularly run both Haskell and CAPE test suites during development
- Use version control to track changes to both spec files and JSON configurations
- Implement automated checks that compare test case coverage between formats
- Maintain clear documentation of test case purposes and expected outcomes

### Code Organization

- Keep fixture data centralized and shared between spec and JSON configurations
- Use consistent naming conventions for test data structures
- Organize test cases by functional area (e.g., deposit, accept, refund operations)
- Separate edge cases and error conditions into clearly labeled sections

## Troubleshooting Workflow Summary

1. **Identify**: Determine which CAPE tests are failing
2. **Reference**: Locate corresponding Haskell spec implementations
3. **Instrument**: Add temporary debug output to capture ScriptContext encodings
4. **Compare**: Analyze differences between Haskell and JSON configurations
5. **Pattern**: Look for systematic issues across multiple test cases
6. **Fix**: Apply consistent corrections to JSON configurations
7. **Verify**: Confirm all tests pass after fixes
8. **Clean**: Remove debug instrumentation and format code

This methodology ensures systematic resolution of test alignment issues while maintaining the principle that Haskell specifications remain the authoritative source of truth for validator behavior.
