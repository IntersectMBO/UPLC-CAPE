# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_0.16.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 0.16.0`
- **Implementation Approach**: `iterative (manual UPLC + pfix Y-combinator)`
- **Compilation Flags**: `default`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

This submission implements the Fibonacci scenario using an iterative approach via manual UPLC construction with a pfix (Y-combinator). The source code is maintained in a separate repository to avoid duplication.

**Note**: The UPLC output uses Scalus bracket notation format, which differs from standard UPLC textual syntax. The verification may require Scalus-specific tooling.
