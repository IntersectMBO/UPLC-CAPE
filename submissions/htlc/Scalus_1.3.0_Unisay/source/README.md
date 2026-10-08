# Scalus HTLC Implementation

**Source Code**: [HTLC.scala](https://github.com/Unisay/scalus-cape-submissions/blob/75678ebd8a1d627102095c4df1b72b774d0ccee8/src/htlc/HTLC.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Branch**: `main`

**Commit**: `75678ebd8a1d627102095c4df1b72b774d0ccee8`

**Path**: `src/htlc/HTLC.scala`

This submission uses Scalus compiler version 1.3.0. HTLC spending validator (`@Compile object HTLCValidator`, `Data -> Unit`) deriving FromData/ToData for `HTLCDatum` and `HTLCRedeemer`. Claim reads the upper bound of `txInfoValidRange` (finite, strictly `< timeout`); refund reads the lower bound (finite, strictly `> timeout`), following the production-safe validity-range convention from IntersectMBO/UPLC-CAPE#170. Targets the van Rossem protocol version (Cardano protocol version 11), live on mainnet since 2026-07-18. Compiled once with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)` (Scala 3.3.8, `scalus-plugin` 1.3.0); there is no separate preview build any more. Compiler changes since 0.18.2 include recursion through self-application instead of the Z combinator, the static-argument transformation, the unified UPLC pipeline, and fee-aware CSE and inlining. The deprecated `findOwnInput` calls are kept, so the delta against 0.18.2 isolates the compiler change. The validator reads only lovelace, which Scalus does not lower to the CIP-153 Value builtins.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 75678ebd8a1d627102095c4df1b72b774d0ccee8
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain htlc.compileHtlc'
   ```

   The `@main` compiles once and writes `src/htlc/htlc.uplc`, which matches the UPLC in this submission.
