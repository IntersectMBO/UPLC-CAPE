# Benchmark Implementation Notes

**Scenario**: `htlc`

**Submission ID**: `Plinth_1.61.0.0_Unisay`

## Implementation Details

- **Compiler**: Plinth (PlutusTx) 1.61.0.0 (preview)
- **Implementation Approach**: High-level PlutusTx API validator with typed datum/redeemer
- **Compilation Flags**: `no-conservative-optimisation`, `no-preserve-logging`, `remove-trace`, `target-version=1.1.0`, `datatypes=BuiltinCasing`

## Architecture

The HTLC validator controls a hashed time-locked payment:

- **Claim** (redeemer = `0(preimage)`): Recipient reveals a preimage whose SHA-256 digest matches the datum hash and withdraws before the timeout
- **Refund** (redeemer = `1()`): Payer reclaims after the timeout has passed

HTLC parameters (payer, recipient, stored hash, timeout) are carried on-chain as inline datum.

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Compilation Config**: Preview build with `BuiltinCasing`, requires plutus-core >= 1.61.0.0 (not yet on mainnet)

## Notes

- Source code: `lib/HTLC.hs`, `lib/HTLC/Fixture.hs`, `lib/Preview/HTLC.hs`
- Recompiled from the same source as the 1.45 submission with `BuiltinCasing` enabled
- Uses `PlutusLedgerApi.V3.Data.Contexts` for `txSignedBy`, SHA-256 preimage check via `sha2_256` builtin
