# Benchmark Implementation Notes

**Scenario**: `two_party_escrow`

**Submission ID**: `Scalus_0.18.2_Unisay`

## Implementation Details

- **Compiler**: `Scalus 0.18.2`
- **Implementation Approach**: `@Compile validator, Data -> Unit, one script with minting and spending purposes (thread token)`
- **Compilation Flags**: `targetProtocolVersion = vanRossemPV` (Cardano protocol version 11)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements
