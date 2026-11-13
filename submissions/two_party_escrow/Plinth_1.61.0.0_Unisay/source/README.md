# two_party_escrow Plinth 1.61.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.61-escrow-thread-token`

**Commit**: `e1f91b9a999d1357bbcccebaa15aa61cfb35444f`

**Path**: `lib/TwoPartyEscrow.hs`

This submission compiles `lib/TwoPartyEscrow.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.61.0.0 line.

The reproduction below uses the preview project file of the source repository, a name kept from the time before the van Rossem hard fork. The artifact runs on the production track: builtin casing has been a mainnet feature since protocol version 11 (2026-07-18).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout e1f91b9a999d1357bbcccebaa15aa61cfb35444f
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
