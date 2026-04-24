# Scalus HTLC Implementation

**Source Code**: [HTLC.scala](https://github.com/Unisay/scalus-cape-submissions/blob/07350edfbaaa3f4c42cf116a3c360ba3098ccf3f/src/htlc/HTLC.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `07350edfbaaa3f4c42cf116a3c360ba3098ccf3f`

**Path**: `src/htlc/HTLC.scala`

This submission uses Scalus compiler version 0.16.0 with a spending validator that enforces the production-safe validity-range convention (claim reads the upper bound, refund reads the lower bound — both finite and strict).

The artifact is compiled with `toUplcOptimized()` (CaseConstrApply + builtin-packing). An AST-level alpha-rename pass rewrites all generated identifiers to short purely-alphabetic names (a, b, …, z, aa, …) before serialisation. This makes the output compatible with the plutus-core 1.45.0.0 textual parser without sacrificing optimised code size.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 07350edfbaaa3f4c42cf116a3c360ba3098ccf3f
   ```

3. Follow build instructions in the repository README

4. The compiled UPLC output should match `htlc.uplc` in this submission
