# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Plinth_1.60.0.0_Unisay_builtincasing` (Format: `Language_Version_GitHubHandle_Variant`)

## Preview Submission

This is a **preview** submission compiled with plutus-core 1.60.0.0, which is
NOT yet deployed on Cardano mainnet. It appears only in the preview report.

## Implementation Details

- **Compiler**: `Plinth 1.60.0.0`
- **Implementation Approach**: `iterative (accumulator-based)`
- **Algorithm Complexity**: O(n) time, O(1) space
- **Compilation Flags**: Standard PlutusTx optimization flags, BuiltinCasing enabled
- **min_plutus_version**: `1.60.0.0`

## Algorithm Description

Same iterative Fibonacci algorithm as the 1.45.0.0 submission, but compiled with
plutus-core 1.60.0.0 to demonstrate the impact of newer compiler optimizations
(including BuiltinCasing) on generated UPLC code.

## Reproducibility

- **Source Available**: true
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Source Location**: `source/FibonacciIterative.hs`
- **Build Tool**: `cabal --project-file=cabal.project.preview run plinth-submissions`
