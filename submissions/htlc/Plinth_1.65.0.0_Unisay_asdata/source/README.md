# htlc Plinth 1.65.0.0 (asdata variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `b77cd0c4987779f8f7d70a1ddd564b8765ecc9a3`

**Path**: `lib/HTLC.hs`

This submission compiles `lib/HTLC.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line.

Datum and redeemer are decoded via `PlutusTx.AsData.asData`-derived pattern matching (no BuiltinCasing); this was the mainnet default implementation strategy prior to the `monadic` variant (Cont-style decoding DSL) overtaking it on fee/size/CPU/mem. Plugin pragmas live in `plinth-cape-submissions.cabal`; validator modules carry no Plinth-specific options.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b77cd0c4987779f8f7d70a1ddd564b8765ecc9a3
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator:

```bash
nix develop
cabal run plinth-submissions
```

The generator at this commit writes `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay/htlc.uplc`, because `asData` was then the default HTLC encoding. That output is the `htlc.uplc` in this submission, which moved to the `asdata` variant when the default changed.
