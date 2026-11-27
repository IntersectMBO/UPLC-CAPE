# htlc Plinth 1.45.0.0 (asdata variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `162d30fb5919d82b3b27d2b5b0b50ffc0b689b25`

**Path**: `lib/HTLC.hs`

This submission compiles `lib/HTLC.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.45.0.0 line.

Datum and redeemer are decoded via `PlutusTx.AsData.asData`-derived pattern matching; this was the mainnet default implementation strategy prior to the `monadic` variant (Cont-style decoding DSL) overtaking it on fee/size/CPU/mem.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 162d30fb5919d82b3b27d2b5b0b50ffc0b689b25
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout. If it is unset, the generator writes to `../UPLC-CAPE`.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.45.0.0_Unisay_asdata/htlc.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `htlc.uplc` in this submission.
