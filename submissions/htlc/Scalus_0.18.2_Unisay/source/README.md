# Scalus HTLC Implementation

**Source Code**: [HTLC.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5/src/htlc/HTLC.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5`

**Path**: `src/htlc/HTLC.scala`

This submission uses Scalus compiler version 0.18.2. HTLC spending validator (`@Compile object HTLCValidator`, `Data -> Unit`) deriving FromData/ToData for `HTLCDatum` and `HTLCRedeemer`. Claim reads the upper bound of `txInfoValidRange` (finite, strictly `< timeout`); refund reads the lower bound (finite, strictly `> timeout`), following the production-safe validity-range convention from IntersectMBO/UPLC-CAPE#170. Compiled with `toUplcOptimized()` (CaseConstrApply + builtin-packing).

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
   sbt 'runMain htlc.compileHtlc'
   ```
