# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Aiken_1.1.24_KtorZ_prepacked`

## Implementation Details

- **Compiler**: `Aiken 1.1.24`
- **Implementation Approach**: `prepacked lookup`
- **Compilation Flags**: N/A
- **Author**: KtorZ

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for the pinned external source and reproducibility instructions

## Notes

Using pre-packed values with fast dynamic selection. Higher size, but constant (and small) evaluation cost.

Rebuilt by Unisay on Aiken 1.1.24 without changing the Aiken implementation. CPU, memory and serialized script size match the historical submission. All `case` scrutinees are `constr` terms; there is no builtin casing or batch-6 builtin, so `min_protocol_version: 11` is not required.
