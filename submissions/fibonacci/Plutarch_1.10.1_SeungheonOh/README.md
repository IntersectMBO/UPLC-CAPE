# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Plutarch_1.10.1_SeungheonOh`

## Implementation Details

- **Compiler**: `Plutarch v1.10.1`
- **Implementation Approach**: `recursive`
- **Compilation Flags**: N/A

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: true
- **Source Repository**: N/A
- **Compilation Config**: describe any non-default parameters

## Notes

I provided two different fix point combinator: `pfix'` and `pfix''`. They have different performance 
trade offs. `pfix'` will generate smaller script with bigger CPU and Memory cost while `pfix''` will 
generate bigger script(in terms of size) with smaller CPU and Memory cost. 
