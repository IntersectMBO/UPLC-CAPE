# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Pebble_0.5.0_michele-nuzzi`

## Implementation Details

- **Compiler**: `Pebble 0.5.0`
- **Implementation Approach**: iterative pair accumulator (O(n) additions)
- **Compilation Flags**: none (default)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

The Pebble `for` loop lowers to a single tail-recursive UPLC function. The artifact uses UPLC 1.2 `case`/`constr` (builtin casing) and therefore requires protocol version 11 (van Rossem); the header stays `1.1.0` per the scenario constraints, matching the other casing submissions in the suite. Negative inputs return the input itself, as the harness expects (`fib(-1) = -1`).
