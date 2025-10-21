# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_0.12.1_nau_prepacked`

## Implementation Details

- **Compiler**: `Scalus 0.12.1`
- **Implementation Approach**: `prepacked`
- **Compilation Flags**: `default`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

This submission implements the Fibonacci scenario using a prepacked optimization approach. The implementation:

- Uses pre-computed Fibonacci numbers stored in a ByteString
- Provides O(1) constant-time lookup performance
- Pre-computes values for fib(0) through fib(25)
- Encodes each Fibonacci number as 3 bytes in big-endian format
- Applies Scalus optimizations including inlining and case-constr transformations

This approach trades program size for evaluation performance, resulting in constant-time execution regardless of the input value (within the pre-computed range).

**Note**: The UPLC output uses Scalus bracket notation format, which differs from standard UPLC textual syntax.
