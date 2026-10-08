# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_1.3.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 1.3.0`
- **Implementation Approach**: `prepacked ByteString lookup table (fib 0..25), O(1) constant-time`
- **Compilation Flags**: `targetProtocolVersion = vanRossemPV` (Cardano protocol version 11)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements
