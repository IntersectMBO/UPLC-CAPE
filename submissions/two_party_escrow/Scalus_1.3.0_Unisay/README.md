# Benchmark Implementation Notes

**Scenario**: `two_party_escrow`

**Submission ID**: `Scalus_1.3.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 1.3.0`
- **Implementation Approach**: `@Compile validator, Data -> Unit, one script with minting and spending purposes (thread token)`
- **Compilation Flags**: `targetProtocolVersion = vanRossemPV` (Cardano protocol version 11)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements
