# Benchmark Implementation Notes

**Scenario**: `fibonacci` **Submission ID**: `Aiken_1.1.17_KtorZ`

## Implementation Details

- **Compiler**: `Aiken v1.1.17`
- **Implementation Approach**: `recursive`
- **Compilation Flags**: N/A

## Performance Results

- CPU Units: `155308959218`
- Memory Units: `559619722`
- Script Size (bytes): `87`
- Term Size: `81`

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: `https://github.com/cardano-scaling/UPLC-CAPE`
- **Compilation Config**: None

## Notes

Most simple / idiomatic implementation I could think of; no particular attempt at optimizing anything.

Use `aiken export` to obtain the raw UPLC for the function; however, I ended up manually removing the Data wrapping / unwrapping which the `export` command adds by default; since Data is used as the primary interface.
