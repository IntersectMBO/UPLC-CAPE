# Benchmark Implementation Notes

**Scenario**: `fibonacci_naive_recursion`

**Submission ID**: `Scalus_0.17.0_Unisay_vanrossem`

## Implementation Details

- **Compiler**: `Scalus 0.17.0`
- **Implementation Approach**: `naive recursive`
- **Compilation Flags**: `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`
- **Track**: preview (`min_plutus_version = 1.60.0.0`)

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

Same naive-recursive Scalus 0.17.0 source as the current-track `Scalus_0.17.0_Unisay/` submission, recompiled with the van Rossem target protocol version to enable `case-on-builtins` and batch-6 builtins. Invalid on mainnet until the van Rossem hard fork (Cardano protocol version 11) activates — projected late-June 2026.
