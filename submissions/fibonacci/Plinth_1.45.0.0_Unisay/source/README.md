# fibonacci Plinth 1.45.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `b09485c75e3ab6b596b9613320abc2b325087612`

**Path**: `lib/FibonacciIterative.hs`

This submission compiles `lib/FibonacciIterative.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.45.0.0 line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b09485c75e3ab6b596b9613320abc2b325087612
nix develop
CAPE_REPO=../UPLC-CAPE cabal run plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/fibonacci/Plinth_1.45.0.0_Unisay/fibonacci.uplc` and matches the `fibonacci.uplc` in this submission.
