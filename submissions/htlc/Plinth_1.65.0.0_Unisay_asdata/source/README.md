# htlc Plinth 1.65.0.0 (asdata variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.65`

**Commit**: `a67fabd5390f69fbf2397281e084ddab7eb12fbd`

**Path**: `lib/HTLC/AsData.hs`

This submission compiles `lib/HTLC/AsData.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line.

Datum and redeemer are decoded via `PlutusTx.AsData.asData`-derived pattern matching (no BuiltinCasing); this was the mainnet default implementation strategy prior to the `monadic` variant (Cont-style decoding DSL) overtaking it on fee/size/CPU/mem. Plugin pragmas live in `plinth-cape-submissions.cabal`; validator modules carry no Plinth-specific options.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout a67fabd5390f69fbf2397281e084ddab7eb12fbd
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout; the generator stops if it is unset. You can also set it in `.envrc.local`, which the branch's `.envrc` loads.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay_asdata/htlc.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `htlc.uplc` in this submission.
