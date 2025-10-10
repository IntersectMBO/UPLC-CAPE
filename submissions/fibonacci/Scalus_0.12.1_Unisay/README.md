# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_0.12.1_Unisay`

## Implementation Details

- **Compiler**: `Scalus 0.12.1`
- **Implementation Approach**: `iterative`
- **Compilation Flags**: `default`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: true
- **Source Repository**: <https://github.com/Unisay/scalus-cape-submissions>
- **Source Path**: `fibonacci/FibonacciOpen.scala`
- **Compilation Config**: default

## Notes

This submission implements the Fibonacci scenario using an iterative approach. This optimization provides better performance than the naive recursive approach. The source code is maintained in a separate repository to avoid duplication.

**Note**: The UPLC output uses Scalus bracket notation format, which differs from standard UPLC textual syntax. The verification may require Scalus-specific tooling.
