# Benchmark Implementation Notes

**Scenario**: `factorial`

**Submission ID**: `Plinth_1.52.0.0_Unisay` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: `Plinth 1.52.0.0`
- **Implementation Approach**: `recursive`
- **Compilation Flags**: Standard Plinth (PlutusTx) optimization flags

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements
- **Result**: `factorial(10) = 3628800` ✓
- **CPU Units**: 5,859,917
- **Memory Units**: 21,751
- **Script Size**: 37 bytes

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Compilation Config**: Targeting Plutus Core 1.1.0 with standard PlutusTx plugin configuration

## Notes

- Uses recursive factorial implementation: `factorial(n) = if n <= 0 then 1 else n * factorial(n-1)`
- Much more efficient than fibonacci due to linear recursion vs exponential
- Compiled with PlutusTx plugin targeting Plutus Core 1.1.0
- Source code available in the `plinth/src/Factorial.hs` module
