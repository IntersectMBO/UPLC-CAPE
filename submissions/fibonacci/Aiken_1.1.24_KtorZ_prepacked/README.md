# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Aiken_1.1.24_KtorZ_prepacked`

**Compiler**: Aiken 1.1.24.

**Implementation**: prepacked lookup.

**Author**: KtorZ.

See [metrics.json](metrics.json) for measurements and [source/README.md](source/README.md) for the pinned external source and build instructions.

Original implementation by KtorZ. Rebuilt by Unisay on Aiken 1.1.24 without changing the Aiken implementation. CPU, memory and serialized script size match the historical submission. All `case` scrutinees are `constr` terms; there is no builtin casing or batch-6 builtin, so `min_protocol_version: 11` is not required.
