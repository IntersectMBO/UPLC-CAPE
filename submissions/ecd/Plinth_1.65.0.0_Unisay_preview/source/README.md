# ecd Plinth 1.65.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `yura/plinth-1.65-preview-submissions`

**Commit**: `3efc19ad785a7dcb6aead5a936d18654f0649a2b`

**Path**: `lib/Ecd.hs`

This submission compiles `lib/Ecd.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line and the BuiltinCasing preview flag enabled. Requires `plutus-core >= 1.65.0.0` (not yet on mainnet).

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

Then enter the dev shell and run the generator with the preview flag:

```bash
nix develop
cabal run --flags=preview plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/ecd/Plinth_1.65.0.0_Unisay_preview/ecd.uplc` and matches the UPLC in this submission.
