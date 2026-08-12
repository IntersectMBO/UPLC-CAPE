# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Plinth_1.61.0.0_Unisay` (Format: `Language_Version_GitHubHandle_Variant`)

## Builtin casing

Compiled with `datatypes=BuiltinCasing`, which mainnet has supported since the
van Rossem hard fork (protocol version 11, 2026-07-18).

## Implementation Details

- **Compiler**: `Plinth 1.61.0.0`
- **Implementation Approach**: `iterative (accumulator-based)`
- **Algorithm Complexity**: O(n) time, O(1) space
- **Compilation Flags**: Standard PlutusTx optimization flags plus `datatypes=BuiltinCasing`

## Algorithm Description

Same iterative Fibonacci algorithm as the 1.45.0.0 submission, but compiled with
the Plinth 1.61.0.0 line to demonstrate the impact of newer compiler optimizations
(including BuiltinCasing) on generated UPLC code.

## Reproducibility

- **Source Available**: true
- **Source Repository**: https://github.com/Unisay/plinth-cape-submissions
- **Source Location**: `lib/FibonacciIterative.hs`
- **Build Tool**: `cabal --project-file=cabal.project.preview run plinth-submissions`
