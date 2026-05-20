# factorial_naive_recursion Plinth 1.61.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.61`

**Commit**: `b09485c75e3ab6b596b9613320abc2b325087612`

**Path**: `lib/Factorial.hs`

This submission compiles `lib/Factorial.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.61.0.0 line.

The `plinth-1.61` branch builds against a newer plutus-core / plutus-tx-plugin line (BuiltinCasing-aware) than the mainnet baseline.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b09485c75e3ab6b596b9613320abc2b325087612
nix develop
CAPE_REPO=../UPLC-CAPE cabal run --project-file=cabal.project.preview -f preview plinth-submissions-preview
```

The produced UPLC writes to `$CAPE_REPO/submissions/factorial_naive_recursion/Plinth_1.61.0.0_Unisay/factorial.uplc` and matches the `factorial.uplc` in this submission.
