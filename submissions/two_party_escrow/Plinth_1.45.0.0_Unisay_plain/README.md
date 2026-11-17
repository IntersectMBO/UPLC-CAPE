# Benchmark Implementation Notes

**Scenario**: `two_party_escrow`

**Submission ID**: `Plinth_1.45.0.0_Unisay_plain` (Format: `Language_Version_GitHubHandle[_variant]`)

## Implementation Details

- **Compiler**: Plinth (PlutusTx) 1.45.0.0
- **Implementation Approach**: hand-written validator on the `PlutusLedgerApi.Data.V3` types, without a decoder DSL
- **Compilation Flags**: `defer-errors`, `no-conservative-optimisation`, `target-version=1.1.0`

## Architecture

One script with the signature `BuiltinData -> BuiltinUnit` runs under two purposes (scenario revision 2):

- **Minting, redeemer 0 (Deposit)**: the buyer mints one thread token and locks it at the script with 75 ADA and the deposit time.
- **Minting, redeemer 1 or 2**: the token is burned.
- **Spending, redeemer 1 (Accept)**: the seller takes the funds and the token is burned.
- **Spending, redeemer 2 (Refund)**: the buyer takes the funds back after the 30-minute deadline and the token is burned.

The script reads its own hash from the script context. The key hashes of the buyer and the seller, the price, the delay and the token name are compiled in.

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: https://github.com/Unisay/plinth-cape-submissions
- **Build Instructions**: See [source/README.md](source/README.md)

## Notes

This is the `plain` variant of the Plinth 1.45.0.0 line. The default row of this line, `Plinth_1.45.0.0_Unisay`, uses the monadic decoder DSL.
