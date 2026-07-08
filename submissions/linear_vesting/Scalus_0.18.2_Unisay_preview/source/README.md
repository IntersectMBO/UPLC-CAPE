# Scalus Linear Vesting Implementation

**Source Code**: [LinearVesting.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5/src/linear_vesting/LinearVesting.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5`

**Path**: `src/linear_vesting/LinearVesting.scala`

This submission uses Scalus compiler version 0.18.2. Linear vesting spending validator (`@Compile object LinearVestingValidator`, `Data -> Unit`) releasing a native asset to a beneficiary on an installment schedule, with all parameters carried in the datum. Redeemer is a nullary constructor (`Constr 0 []` = PartialUnlock, `Constr 1 []` = FullUnlock; a raw-integer redeemer is rejected). Preview submission targeting the van Rossem hard fork (Cardano protocol version 11). Same source as the corresponding current-track Scalus_0.18.2_Unisay submission, recompiled with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`, which enables `case-on-builtins` and batch-6 builtins (e.g. `dropList`). Measured on `PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)`. Invalid on mainnet until the hard fork activates; gated by `min_plutus_version = 1.60.0.0` for measurement and verification, while the `_preview` directory variant places it in the preview report track.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain linear_vesting.compileLinearVesting'
   ```

   The `@main` writes both the changPV artifact (`linear_vesting`) and the `-preview.uplc` into `src/linear_vesting/`; this submission pins the `-preview.uplc` build.
