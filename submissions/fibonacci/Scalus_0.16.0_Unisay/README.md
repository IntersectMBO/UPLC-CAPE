# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_0.16.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 0.16.0`
- **Implementation Approach**: `naive recursive via explicit pfix (Y-combinator)`
- **Compilation Flags**: `default`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

Surface Scalus defines fibonacci as `pfix: r => λ x. if x ≤ 1 then x else r(x-1) + r(x-2)` — the classic naive O(2^n) recurrence with an explicit Y-combinator (`pfix`), not an iterative or tail-recursive form. The source code is maintained in a separate repository to avoid duplication.
