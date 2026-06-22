# Benchmark Implementation Notes

**Scenario**: `linear_vesting`

**Submission ID**: `Plinth_1.45.0.0_Unisay`

## Implementation Details

- **Compiler**: Plinth (PlutusTx) 1.45.0.0
- **Implementation Approach**: High-level PlutusTx API validator with typed datum/redeemer
- **Compilation Flags**: `no-conservative-optimisation`, `target-version=1.1.0`

## Architecture

The Linear Vesting validator controls gradual release of tokens over a time schedule:

- **PartialUnlock** (redeemer = 0()): Withdraw proportional tokens during vesting period
- **FullUnlock** (redeemer = 1()): Withdraw all remaining tokens after vesting period ends

Vesting parameters (beneficiary, asset, schedule) are stored on-chain as inline datum, not baked into the compiled UPLC. The validator reads datum from ScriptInfo at runtime.

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Compilation Config**: PlutusTx with no-conservative-optimisation, targeting Plutus Core 1.1.0

## Notes

- Source code: `lib/LinearVesting.hs` and `lib/LinearVesting/Fixture.hs`
- Adapted from `plutus-benchmark/linear-vesting` using high-level typed PlutusTx APIs
- Uses `PlutusLedgerApi.V3.Data.Contexts` for `findOwnInput`, `getContinuingOutputs`, `txSignedBy`
- Downgraded to Plutus 1.45 for mainnet compatibility (cardano-node 10.4.1)
