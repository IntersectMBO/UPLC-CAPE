# Benchmark Implementation Notes

**Scenario**: `factorial`

**Submission ID**: `Scalus_0.16.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 0.16.0`
- **Implementation Approach**: `simple recursive via explicit pfix (Y-combinator)`
- **Compilation Flags**: `default`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

Surface Scalus defines factorial as `pfix: r => λ x. if x ≤ 0 then 1 else x * r(x-1)` — plain recursion with an explicit Y-combinator (`pfix`), no accumulator, no tail-recursive rewrite. The source code is maintained in a separate repository to avoid duplication.
