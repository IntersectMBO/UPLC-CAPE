# Two-Party Escrow Benchmark Scenario

## Overview

The Two-Party Escrow benchmark is a **real-world smart contract scenario** designed to measure the performance characteristics of validator implementations as UPLC programs. This benchmark tests a compiler's ability to optimize conditional logic, data structure access, cryptographic operations, and state transitions in a practical escrow contract.

## TL;DR

Implement a two-party escrow validator that handles deposit, accept, and refund operations and compile it as a fully-applied UPLC program.

**Required Files**: Submit `two-party-escrow.uplc`, `metadata.json`, `metrics.json` to `submissions/two-party-escrow/{Compiler}_{Version}_{Handle}/`

**Target**: Accept sequence → Expected result: `() (unit)`  
**Metrics**: CPU units, Memory units, Script size (bytes), Term size  
**Constraints**: Plutus Core 1.1.0, Plutus V3 recommended, CEK machine budget limits  
**Implementation**: Handle deposit, accept, and refund with proper validation

---

## Exact Task

Implement a two-party escrow validator and compile it as a **fully-applied UPLC program** that processes escrow transactions through deposit, accept, and refund operations.

### Core Requirements

1. **Validator Implementation**: Create a validator with signature `BuiltinData -> BuiltinUnit` that handles three redeemer types:

   - `Deposit` (redeemer = 0): Allow buyer to deposit funds into escrow
   - `Accept` (redeemer = 1): Allow seller to accept and complete the escrow
   - `Refund` (redeemer = 2): Allow buyer to reclaim funds after deadline

2. **Fixed Parameters**: The following parameters must be baked into the UPLC program:

   - **Buyer Address**: `a1b2c3d4e5f6789012345678abcdef0123456789abcdef0123456789abcdef01`
   - **Seller Address**: `fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210`
   - **Price**: 75 ADA (75,000,000 lovelace)
   - **Deadline**: 30 minutes after deposit (1800 seconds)

3. **State Transitions**: The validator must enforce proper state transitions and validation rules

### Redeemer Encoding

```text
Deposit = 0  -- Buyer deposits 75 ADA into escrow
Accept  = 1  -- Seller accepts and receives payment
Refund  = 2  -- Buyer reclaims funds after deadline
```

---

## View 1: State Lifecycle View

The Two-Party Escrow validator operates as a **state machine validator**:

```text
[Initial] --Deposit(0)--> [DepositValid] --Accept(1)--> [AcceptComplete]
                                     \
                                      --Refund(2)--> [RefundComplete]
```

| Current State | Event | Condition | Next State |
| --- | --- | --- | --- |
| **Initial** | `Deposit(0)` | Buyer deposits 75 ADA | **DepositValid** |
| **DepositValid** | `Accept(1)` | Seller accepts, funds to seller | **AcceptComplete** |
| **DepositValid** | `Refund(2)` | Past deadline, funds to buyer | **RefundComplete** |
| **AcceptComplete** | - | Escrow completed successfully | **Final** |
| **RefundComplete** | - | Escrow refunded | **Final** |

**State Descriptions**:

- **Initial**: No funds deposited, waiting for buyer deposit
- **DepositValid**: Buyer has deposited 75 ADA, awaiting seller acceptance or deadline
- **AcceptComplete**: Seller has accepted payment, escrow complete
- **RefundComplete**: Buyer has reclaimed funds after deadline
- **Final**: Terminal state, no further transactions

**Note**: Each state transition must validate appropriate signatures, values, and timing constraints

---

## View 2: Transaction Sequence View

### Performance Measurement Sequence (Happy Path)

The **Accept Sequence** is used for performance measurement:

1. **Initial → DepositValid**: Buyer deposits 75 ADA
2. **DepositValid → AcceptComplete**: Seller accepts payment

**Performance Measurement**: Sum of CPU/Memory units across both steps

### Verification Sequences

**Refund Sequence** (functional verification only):

1. **Initial → DepositValid**: Buyer deposits 75 ADA
2. **DepositValid → RefundComplete**: Buyer reclaims funds after deadline

**Extended Negative Test Sequences**:

- Authorization violations (wrong signatures)
- Temporal violations (refund before deadline)
- Value violations (incorrect amounts)
- State transition violations (invalid sequences)

---

## Implementation Requirements

### Technical Constraints

1. **Execution Budget**: Each transaction step must complete within CEK machine limits
2. **Determinism**: Results must be identical across multiple executions
3. **Self-Contained**: All parameters (addresses, price, deadline) baked into UPLC program
4. **Correctness**: Must enforce all validation rules correctly
5. **Signature**: Validator function type `BuiltinData -> BuiltinUnit`

### Validation Rules

#### Deposit Operation (Redeemer = 0)

- **Value Check**: Exactly 75 ADA deposited to script address
- **Authorization**: Transaction signed by buyer
- **State Check**: No existing deposit (initial state)

#### Accept Operation (Redeemer = 1)

- **Value Check**: 75 ADA paid to seller address
- **Authorization**: Transaction signed by seller
- **State Check**: Valid deposit exists
- **Timing**: No deadline restriction for acceptance

#### Refund Operation (Redeemer = 2)

- **Value Check**: 75 ADA returned to buyer address
- **Authorization**: Transaction signed by buyer
- **State Check**: Valid deposit exists
- **Timing**: Current time > deposit time + 1800 seconds

### Performance Characteristics

**Expected Ranges** (approximate guidelines for reference implementations):

- CPU Units per operation: 50,000 - 500,000
- Memory Units per operation: 10,000 - 100,000
- Script Size: 1,000 - 10,000 bytes
- Accept Sequence Total: 100,000 - 1,000,000 CPU units

_Note: These ranges are indicative and may vary significantly based on compiler optimizations and implementation approaches._

### Validation

To validate your implementation:

1. **Functional Test**: Ensure all three operations validate correctly
2. **Budget Test**: Verify each operation completes within CEK machine limits
3. **Sequence Test**: Test complete Accept and Refund sequences
4. **Negative Test**: Verify proper rejection of invalid transactions

---

## Test Cases

### Primary Test Case (Performance Measurement)

**Accept Sequence**:

1. **Input**: Redeemer = 0 (Deposit) **Expected**: Validation succeeds
2. **Input**: Redeemer = 1 (Accept) **Expected**: Validation succeeds

**Combined Performance**: Sum CPU/Memory units from both operations

### Functional Verification Cases

**Refund Sequence**:

1. **Input**: Redeemer = 0 (Deposit) **Expected**: Validation succeeds
2. **Input**: Redeemer = 2 (Refund) **Expected**: Validation succeeds (after deadline)

### Edge Case Considerations

- **Authorization Failures**: Wrong signature on any operation
- **Timing Violations**: Refund attempt before deadline expiration
- **Value Mismatches**: Incorrect amounts in deposit/payment/refund
- **State Violations**: Accept/refund without valid deposit
- **Double Spending**: Multiple operations on same UTXO

---

## Measurement Guidelines

### Required Metrics

All submissions must include measurements for the **Accept Sequence**:

1. **CPU Units**: Total computational cost (sum of Deposit + Accept)
2. **Memory Units**: Peak memory consumption across both operations
3. **Script Size**: Size of the compiled UPLC validator script in bytes
4. **Term Size**: Size of the UPLC term representation

### Measurement Method

**Happy Path Measurement**:

1. Execute Deposit operation (redeemer = 0), record CPU/Memory
2. Execute Accept operation (redeemer = 1), record CPU/Memory
3. Sum the values for total sequence cost

### Reporting Format

Use the standard metrics template in `submissions/TEMPLATE/metrics-template.json`:

```json
{
  "scenario": "two-party-escrow",
  "version": "1.0.0",
  "measurements": {
    "cpu_units": "<sum_of_deposit_and_accept_cpu>",
    "memory_units": "<peak_memory_across_operations>",
    "script_size_bytes": "<validator_script_size>",
    "term_size": "<uplc_term_size>"
  },
  "execution_environment": {
    "evaluator": "<evaluator_version>"
  },
  "sequence_measured": "accept_sequence",
  "timestamp": "<ISO-8601_timestamp>",
  "measurement_method": "manual",
  "notes": "Measured happy path: Deposit + Accept operations"
}
```

---

## Implementation Notes

### Algorithm Considerations

- **Data Encoding**: Efficient encoding of redeemer integers and validation context
- **Signature Verification**: Optimize cryptographic operations if available
- **Time Handling**: Efficient deadline comparison logic
- **Value Validation**: Streamlined ADA amount checking

### Common Pitfalls

- **Integer Overflow**: Ensure safe arithmetic operations for large lovelace values
- **Redeemer Parsing**: Proper decoding of BuiltinData to integer redeemer
- **State Consistency**: Maintain consistent view of escrow state across operations
- **Deadline Logic**: Correct time comparison with proper bounds checking

### Optimization Opportunities

- **Branch Prediction**: Optimize common paths (Accept more likely than Refund)
- **Data Structure Access**: Minimize BuiltinData traversal costs
- **Constant Folding**: Leverage fixed parameters for compile-time optimizations
- **Dead Code Elimination**: Remove unused validation paths

---

## Security Considerations

### Validation Requirements

- **Cryptographic Integrity**: Proper signature verification
- **Economic Safety**: Prevent value leakage or double spending
- **Temporal Safety**: Enforce deadline constraints correctly
- **Authorization Safety**: Verify proper transaction signers

### Attack Vectors to Consider

- **Race Conditions**: Simultaneous accept/refund attempts
- **Replay Attacks**: Reusing valid transaction patterns
- **Time Manipulation**: Exploiting deadline calculation edge cases
- **Value Manipulation**: Attempts to extract more/less than 75 ADA

---

## References and Resources

### Related Work

- **Cardano Escrow Patterns**: Real-world escrow implementations on Cardano
- **Plutus Validator Examples**: Reference implementations in Plutus ecosystem

### Helpful Documentation

- [Plutus Validator Documentation](https://plutus.cardano.intersectmbo.org/docs/)
- [Cardano Transaction Model](https://docs.cardano.org/learn/cardano-node/)
- [UPLC Specification](https://plutus.readthedocs.io/)
- [CEK Machine Documentation](https://plutus.readthedocs.io/)

---

_This scenario is part of the CAPE (Comparative Artifact Performance Evaluation) framework for benchmarking UPLC compiler performance, focusing on real-world smart contract validator patterns._
