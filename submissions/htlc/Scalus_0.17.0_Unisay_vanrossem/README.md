# Benchmark Implementation Notes

**Scenario**: `htlc`

**Submission ID**: `Scalus_0.17.0_Unisay_vanrossem`

## Implementation Details

- **Compiler**: `Scalus 0.17.0`
- **Implementation Approach**: `idiomatic @Compile spending validator, Data -> Unit, derived FromData/ToData`
- **Compilation Flags**: `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`
- **Track**: preview (`min_plutus_version = 1.60.0.0`)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

Same Scalus 0.17.0 HTLC validator source as the current-track `Scalus_0.17.0_Unisay/` submission (production-safe validity-range convention from #170: claim reads upper bound, refund reads lower bound — both finite, strict), recompiled with the van Rossem target protocol version to enable `case-on-builtins` and batch-6 builtins. CPU/MEM measured against a minimal `ScriptContext` harness (`src/htlc/HtlcHarness.scala`) covering both `Claim` and `Refund` redeemers. Invalid on mainnet until the van Rossem hard fork (Cardano protocol version 11) activates — projected late-June 2026.
