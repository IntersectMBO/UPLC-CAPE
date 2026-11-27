# fibonacci Plinth 1.61.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.61`

**Commit**: `cde46fe7a61f966e88467f256184a9f1de7c1306`

**Path**: `lib/FibonacciIterative.hs`

This submission compiles `lib/FibonacciIterative.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.61.0.0 line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout cde46fe7a61f966e88467f256184a9f1de7c1306
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout. If it is unset, the generator writes to `../UPLC-CAPE`.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/fibonacci/Plinth_1.61.0.0_Unisay/fibonacci.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `fibonacci.uplc` in this submission.
