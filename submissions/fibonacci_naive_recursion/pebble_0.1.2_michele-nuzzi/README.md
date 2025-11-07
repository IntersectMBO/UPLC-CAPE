# Benchmark Implementation Notes

**Scenario**: `fibonacci_naive_recursion`

**Submission ID**: `pebble_0.1.2_michele-nuzzi` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: `pebble 0.1.2`
- **Implementation Approach**: `recursive`
- **Compilation Flags**: none

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

install pebble using
```bash
bun i -g @harmoniclabs/pebble-cli@0.1.2
```
verify the compiler version matches as following
```bash
pebble --version
```
```
pebble-cli version:      0.1.2
pebble language version: 0.1.2
commit hash:             40887038e5d00f171e4684f4c456ffd468008445
```

### Source Code

- **Source Available**: `true`

#### For Inline Source (if using `source/` directory):

- **Source Location**: `source/` directory in this submission
- **Build Instructions**:

in this directory, run
```bash
pebble export --function-name fib --entry source/fib.pebble --output out/out.flat
```
in `out/out.flat` you'll find the serialized function in flat format

to extract the textual uplc run

```bash
rm fibonacci_naive_recursion.uplc # removes current template
aiken uplc decode out/out.flat >> fibonacci_naive_recursion.uplc
```

### Compilation Configuration

none (default)