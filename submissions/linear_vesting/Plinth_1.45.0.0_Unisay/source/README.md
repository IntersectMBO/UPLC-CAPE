# linear_vesting Plinth 1.45.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `b09485c75e3ab6b596b9613320abc2b325087612`

**Path**: `lib/LinearVesting.hs`

This submission compiles `lib/LinearVesting.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.45.0.0 line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b09485c75e3ab6b596b9613320abc2b325087612
nix develop
CAPE_REPO=../UPLC-CAPE cabal run plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/linear_vesting/Plinth_1.45.0.0_Unisay/linear_vesting.uplc` and matches the `linear_vesting.uplc` in this submission.
