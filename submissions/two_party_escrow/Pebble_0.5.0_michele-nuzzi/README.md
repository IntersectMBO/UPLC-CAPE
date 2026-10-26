# Benchmark Implementation Notes

**Scenario**: `two_party_escrow`

**Submission ID**: `Pebble_0.5.0_michele-nuzzi`

## Implementation Details

- **Compiler**: `Pebble 0.5.0`
- **Implementation Approach**: fully-applied `(data) -> unit` validator; single pass over the outputs
- **Compilation Flags**: none (default)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

Deposit / Accept / Refund dispatch on the raw integer redeemer. Accept and refund validate the exact payment and the no-remaining-script-output rule in ONE traversal of the outputs (`paidClean`); deposit reads the validity range's upper bound (must be finite) and requires exactly the price at the script credential. The datum decodes lazily, so a malformed or missing escrow datum fails exactly where the tests expect. Requires protocol version 11 (van Rossem).
