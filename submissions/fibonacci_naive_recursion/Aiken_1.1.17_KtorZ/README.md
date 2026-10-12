# Benchmark Implementation Notes

**Scenario**: `fibonacci_naive_recursion`

**Submission ID**: `Aiken_1.1.17_KtorZ`

## Implementation Details

- **Compiler**: `Aiken 1.1.17`
- **Implementation Approach**: `naive recursion`
- **Compilation Flags**: N/A
- **Author**: KtorZ

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for the pinned external source and reproducibility instructions

## Notes

Most simple / idiomatic implementation I could think of; no particular attempt at optimizing anything.

Use `aiken export` to obtain the raw UPLC for the function; however, I ended up manually removing the Data wrapping / unwrapping which the `export` command adds by default; since Data is used as the primary interface.
