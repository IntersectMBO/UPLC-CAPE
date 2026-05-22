# htlc Plinth 1.64.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `fbb37ddd6ba6ea6fd29312494a32a3074ba332b0`

**Path**: `lib/HTLC.hs`

This submission compiles `lib/HTLC.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.64.0.0 line and the BuiltinCasing preview flag enabled. Requires `plutus-core >= 1.64.0.0` (not yet on mainnet).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout fbb37ddd6ba6ea6fd29312494a32a3074ba332b0
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

The produced UPLC writes to `$CAPE_REPO/submissions/htlc/Plinth_1.64.0.0_Unisay_builtincasing/htlc.uplc` and matches the UPLC in this submission.
