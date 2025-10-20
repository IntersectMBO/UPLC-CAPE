# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Aiken_1.1.19_KtorZ_tailrec` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: `Aiken v1.1.9`
- **Implementation Approach**: `(tail) recursive`
- **Compilation Flags**: N/A

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

### Source Code

- **Source Available**: `true`
- **Source Location**: [`source/`](./source/lib/fibonacci.ak) directory in this submission
- **Build Instructions**: `aiken export --module fibonacci --name fibonacci`

## Notes

A tail-rec optimised implementation, building a bit of the result on each iteration.
