# linear_vesting Plinth 1.45.0.0 (plain variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `162d30fb5919d82b3b27d2b5b0b50ffc0b689b25`

**Path**: `lib/LinearVesting.hs`

This submission compiles `lib/LinearVesting.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.45.0.0 line.

Production line; mainnet plutus-core baseline.

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

The generator writes `$CAPE_REPO/submissions/linear_vesting/Plinth_1.45.0.0_Unisay_plain/linear_vesting.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `linear_vesting.uplc` in this submission.

> Retained as the `plain` variant: this was the default implementation for the Plinth 1.45.0.0 line until the monadic decoder became the default (lower fee, size, CPU, and memory).
