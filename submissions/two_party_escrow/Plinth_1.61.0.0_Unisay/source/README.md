# two_party_escrow Plinth 1.61.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.61-escrow-30min`

**Commit**: `d0a9d341a4702ef572c16e223f5cb55857d7a622`

**Path**: `lib/TwoPartyEscrow.hs`

This submission compiles `lib/TwoPartyEscrow.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.61.0.0 line.

The `plinth-1.61` branch builds against a newer plutus-core / plutus-tx-plugin line (BuiltinCasing-aware) than the mainnet baseline.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout d0a9d341a4702ef572c16e223f5cb55857d7a622
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator:

```bash
nix develop
cabal run --project-file=cabal.project.preview -f preview plinth-submissions-preview
```

The produced UPLC writes to `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.61.0.0_Unisay/two_party_escrow.uplc` and matches the `two_party_escrow.uplc` in this submission.
