# linear_vesting Plinth 1.61.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.61`

**Commit**: `b09485c75e3ab6b596b9613320abc2b325087612`

**Path**: `lib/LinearVesting.hs`

This submission compiles `lib/LinearVesting.hs` from the Plinth source
repository with the Plinth (plutus-tx-plugin) 1.61.0.0 line.

The `plinth-1.61` branch builds against a newer plutus-core / plutus-tx-plugin line (BuiltinCasing-aware) than the mainnet baseline.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b09485c75e3ab6b596b9613320abc2b325087612
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the
build aborts if the variable is unset. The recommended place is
`.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator:

```bash
nix develop
cabal run --project-file=cabal.project.preview -f preview plinth-submissions-preview
```

The produced UPLC writes to
`$CAPE_REPO/submissions/linear_vesting/Plinth_1.61.0.0_Unisay/linear_vesting.uplc`
and matches the `linear_vesting.uplc` in this submission.
