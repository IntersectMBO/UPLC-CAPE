# factorial_naive_recursion Plinth 1.64.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `5c014861ac6a7a0e85234ab8a7369a0c8f2adf20`

**Path**: `lib/Factorial.hs`

This submission compiles `lib/Factorial.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.64.0.0 line and the BuiltinCasing preview flag enabled. Requires `plutus-core >= 1.64.0.0` (not yet on mainnet).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 5c014861ac6a7a0e85234ab8a7369a0c8f2adf20
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator with the preview flag:

```bash
nix develop
cabal run --flags=preview plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/factorial_naive_recursion/Plinth_1.64.0.0_Unisay_builtincasing/factorial.uplc` and matches the UPLC in this submission.
