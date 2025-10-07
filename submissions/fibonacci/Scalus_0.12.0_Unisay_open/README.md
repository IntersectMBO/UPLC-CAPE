# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_0.12.0_Unisay_open`

## Implementation Details

- **Compiler**: `Scalus 0.12.0`
- **Implementation Approach**: `iterative`
- **Compilation Flags**: `open mode`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: true
- **Source Repository**: <https://github.com/Unisay/scalus-cape-submissions>
- **Source Path**: `fibonacci/open/FibonacciOpen.scala`
- **Compilation Config**: open mode

## Notes

This submission implements the Fibonacci scenario using an iterative approach with Scalus open mode configuration. Open mode provides slightly better performance than the recursive base mode approach. The source code is maintained in a separate repository to avoid duplication.

**Note**: The UPLC output uses Scalus bracket notation format, which differs from standard UPLC textual syntax. The verification may require Scalus-specific tooling.
