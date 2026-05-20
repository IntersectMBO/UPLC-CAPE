# htlc Plinth 1.64.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `5c014861ac6a7a0e85234ab8a7369a0c8f2adf20`

**Path**: `lib/HTLC.hs`

This submission compiles `lib/HTLC.hs` from the Plinth source
repository with the Plinth (plutus-tx-plugin) 1.64.0.0 line (production,
no BuiltinCasing). The plugin pragmas live in
`plinth-cape-submissions.cabal`; validator modules carry no
Plinth-specific options.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 5c014861ac6a7a0e85234ab8a7369a0c8f2adf20
nix develop
export CAPE_REPO=../UPLC-CAPE
cabal run plinth-submissions
```

The produced UPLC writes to
`$CAPE_REPO/submissions/htlc/Plinth_1.64.0.0_Unisay/htlc.uplc`
and matches the `htlc.uplc` in this submission.
