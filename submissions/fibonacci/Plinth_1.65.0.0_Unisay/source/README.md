# fibonacci Plinth 1.65.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `3efc19ad785a7dcb6aead5a936d18654f0649a2b`

**Path**: `lib/FibonacciIterative.hs`

This submission compiles `lib/FibonacciIterative.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line and the `datatypes=BuiltinCasing` plugin flag. Builtin casing has been a mainnet feature since the van Rossem hard fork (protocol version 11, 2026-07-18).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 3efc19ad785a7dcb6aead5a936d18654f0649a2b
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator with the casing build flag (the source repo still gates it behind `preview` at this commit):

```bash
nix develop
cabal run --flags=preview plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/fibonacci/Plinth_1.65.0.0_Unisay/fibonacci.uplc` and matches the UPLC in this submission.
