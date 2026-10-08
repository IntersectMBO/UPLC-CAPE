# Benchmark Implementation Notes

**Scenario**: `two_party_escrow`

**Submission ID**: `Scalus_1.3.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 1.3.0`
- **Implementation Approach**: `@Compile spending validator, Data -> Unit, Deposited->Accepted|Refunded state machine`
- **Compilation Flags**: `targetProtocolVersion = vanRossemPV` (Cardano protocol version 11)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements
