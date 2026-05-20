# factorial_naive_recursion Plinth 1.45.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `b09485c75e3ab6b596b9613320abc2b325087612`

**Path**: `lib/Factorial.hs`

This submission compiles `lib/Factorial.hs` from the Plinth source
repository with the Plinth (plutus-tx-plugin) 1.45.0.0 line.

Production line; mainnet plutus-core baseline.

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
cabal run plinth-submissions
```

The produced UPLC writes to
`$CAPE_REPO/submissions/factorial_naive_recursion/Plinth_1.45.0.0_Unisay/factorial.uplc`
and matches the `factorial.uplc` in this submission.
