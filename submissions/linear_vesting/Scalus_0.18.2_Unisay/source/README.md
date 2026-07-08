# Scalus Linear Vesting Implementation

**Source Code**: [LinearVesting.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5/src/linear_vesting/LinearVesting.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5`

**Path**: `src/linear_vesting/LinearVesting.scala`

This submission uses Scalus compiler version 0.18.2. Linear vesting spending validator (`@Compile object LinearVestingValidator`, `Data -> Unit`) releasing a native asset to a beneficiary on an installment schedule, with all parameters carried in the datum. Redeemer is a nullary constructor (`Constr 0 []` = PartialUnlock, `Constr 1 []` = FullUnlock; a raw-integer redeemer is rejected). Compiled with `Options.release` (the Scalus release optimizer).

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
