# ecd Plinth 1.64.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64`

**Commit**: `1999e3dc700416dce562dd2b841700ccdaa4424c`

**Path**: `lib/Ecd.hs`

This submission compiles `lib/Ecd.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.64.0.0 line and the `datatypes=BuiltinCasing` plugin flag. Builtin casing has been a mainnet feature since the van Rossem hard fork (protocol version 11, 2026-07-18).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 1999e3dc700416dce562dd2b841700ccdaa4424c
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout; the generator stops if it is unset. You can also set it in `.envrc.local`, which the branch's `.envrc` loads.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run --flags=builtincasing plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/ecd/Plinth_1.64.0.0_Unisay/ecd.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `ecd.uplc` in this submission.
