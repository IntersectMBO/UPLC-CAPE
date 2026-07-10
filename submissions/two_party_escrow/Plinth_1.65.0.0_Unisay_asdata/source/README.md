# two_party_escrow Plinth 1.65.0.0 (asdata variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `c663b170bb2148a20072d8400b8679c03c11de4c`

**Path**: `lib/TwoPartyEscrow/AsData.hs`

This submission compiles `lib/TwoPartyEscrow/AsData.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line.

Production line with Plinth 1.65.0.0 (no BuiltinCasing). Plugin pragmas live in `plinth-cape-submissions.cabal`; validator modules carry no Plinth-specific options.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout c663b170bb2148a20072d8400b8679c03c11de4c
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator:

```bash
nix develop
cabal run plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.65.0.0_Unisay_asdata/two_party_escrow.uplc` and matches the `two_party_escrow.uplc` in this submission.

> Retained as the `asdata` variant: this was the default implementation for the Plinth 1.65.0.0 line until the monadic decoder became the default (lower fee, size, CPU, and memory).
