# Benchmark Implementation Notes

**Scenario**: `htlc`

**Submission ID**: `Plinth_1.45.0.0_Unisay_asdata`

## Implementation Details

- **Compiler**: Plinth (PlutusTx) 1.45.0.0
- **Implementation Approach**: High-level PlutusTx API validator with typed datum/redeemer
- **Compilation Flags**: `no-conservative-optimisation`, `target-version=1.1.0`

## Architecture

The HTLC validator controls a hashed time-locked payment:

- **Claim** (redeemer = `0(preimage)`): Recipient reveals a preimage whose SHA-256 digest matches the datum hash and withdraws before the timeout
- **Refund** (redeemer = `1()`): Payer reclaims after the timeout has passed

HTLC parameters (payer, recipient, stored hash, timeout) are carried on-chain as inline datum, not baked into the compiled UPLC. The validator reads datum from ScriptInfo at runtime.

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Compilation Config**: PlutusTx with no-conservative-optimisation, targeting Plutus Core 1.1.0

## Notes

- Source code: `lib/HTLC.hs` and `lib/HTLC/Fixture.hs`
- Datum and redeemer are encoded with [`PlutusTx.AsData.asData`](https://plutus.cardano.intersectmbo.org/docs/working-with-scripts/optimizing-scripts-with-asData) so field extraction is lazy; the validator pattern-matches on `ScriptContext` / `TxInfo` / `TxOut` / `Address` exactly once each (no field accessors on `asData` types) to avoid re-decoding the underlying `Data`
- SHA-256 preimage check via `sha2_256` builtin
- Plinth 1.45 stack for mainnet compatibility (cardano-node 10.4.1)
