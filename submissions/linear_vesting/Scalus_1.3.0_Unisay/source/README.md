# Scalus Linear Vesting Implementation

**Source Code**: [LinearVesting.scala](https://github.com/Unisay/scalus-cape-submissions/blob/75678ebd8a1d627102095c4df1b72b774d0ccee8/src/linear_vesting/LinearVesting.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Branch**: `main`

**Commit**: `75678ebd8a1d627102095c4df1b72b774d0ccee8`

**Path**: `src/linear_vesting/LinearVesting.scala`

This submission uses Scalus compiler version 1.3.0. Linear vesting spending validator (`@Compile object LinearVestingValidator`, `Data -> Unit`) releasing a native asset to a beneficiary on an installment schedule, with all parameters carried in the datum. Redeemer is a nullary constructor (`Constr 0 []` = PartialUnlock, `Constr 1 []` = FullUnlock; a raw-integer redeemer is rejected). Targets the van Rossem protocol version (Cardano protocol version 11), live on mainnet since 2026-07-18. Compiled once with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)` (Scala 3.3.8, `scalus-plugin` 1.3.0); there is no separate preview build any more. Compiler changes since 0.18.2 include recursion through self-application instead of the Z combinator, the static-argument transformation, the unified UPLC pipeline, and fee-aware CSE and inlining. The deprecated `findOwn*` / `getValidityStartTime` calls are kept, so the delta against 0.18.2 isolates the compiler change. `quantityOf` now lowers to the CIP-153 `lookupCoin` over `unValueData`, which requires canonical Values. `divCeil` calls `divideInteger` explicitly, because `BigInt /` truncates (`quotientInteger`) since Scalus 1.0; the semantics match 0.18.2.

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
   sbt 'runMain linear_vesting.compileLinearVesting'
   ```

   The `@main` compiles once and writes `src/linear_vesting/linear_vesting.uplc`, which matches the UPLC in this submission.
