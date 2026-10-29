# Benchmark Implementation Notes

**Scenario**: `factorial`

**Submission ID**: `Pebble_0.5.0_michele-nuzzi`

## Implementation Details

- **Compiler**: `Pebble 0.5.0`
- **Implementation Approach**: iterative accumulator (O(n) multiplications)
- **Compilation Flags**: none (default)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

The Pebble `for` loop lowers to a single tail-recursive UPLC function. The artifact uses UPLC 1.2 `case`/`constr` (builtin casing) and therefore requires protocol version 11 (van Rossem). Inputs `n <= 0` return 1, as the scenario specifies.
