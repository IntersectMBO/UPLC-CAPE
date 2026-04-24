# Scalus HTLC Implementation

**Source Code**: [HTLC.scala](https://github.com/Unisay/scalus-cape-submissions/blob/9cdace4a7369954f470d73a58f3c902e4427c439/src/htlc/HTLC.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `9cdace4a7369954f470d73a58f3c902e4427c439`

**Path**: `src/htlc/HTLC.scala`

This submission uses Scalus compiler version 0.16.0 with a spending validator that enforces the production-safe validity-range convention (claim reads the upper bound, refund reads the lower bound — both finite and strict). Two workarounds are applied to make the UPLC compatible with the plutus-core 1.45.0.0 textual parser:

1. `toUplc()` instead of `toUplcOptimized()` — avoids `case`/`constr` syntax from CaseConstrApply (upstream: [IntersectMBO/plutus#7742](https://github.com/IntersectMBO/plutus/issues/7742)).
2. Post-processing renames `NAME-NNNNrMMMM` identifiers to `NAME_NNNNrMMMM` — the plutus-core unique-suffix parser uses `Lex.decimal` after a hyphen, which stops at `r`, causing the refresh-counter suffix to be misread as the lambda body.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 0846510184d98b054c4405c4d036154444467d9f
   ```

3. Follow build instructions in the repository README

4. The compiled UPLC output should match `htlc.uplc` in this submission
