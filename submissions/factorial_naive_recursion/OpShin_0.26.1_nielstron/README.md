# Benchmark Implementation Notes

**Scenario**: `factorial_naive_recursion`

**Submission ID**: `OpShin_0.26.1_nielstron`

## Implementation Details

- **Compiler**: `OpShin`
- **Implementation Approach**: naive recursion as prescribed.

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

### Source Code

[`src/factorial_naive_recursion/contract.py`](https://github.com/OpShin/opshin-cape-submissions/blob/e9d934532514e956425b16b630a584f060f91250/src/factorial_naive_recursion/contract.py)

## Notes

The compiler is not a tagged OpShin release. It is built from master at commit `db8a9858d453ec625e2a3ce1be98130cf20abbb2`, 35 commits after the 0.26.1 tag and before 0.27.0. The `pyproject.toml` at that commit still reads 0.26.1, which is the version recorded in `metadata.json`.
