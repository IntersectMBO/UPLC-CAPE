# two_party_escrow Plinth 1.64.0.0 (plain variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64`

**Commit**: `63eaf4b8cb94e6d5be4350fcc481152dca94b15f`

**Path**: `lib/TwoPartyEscrow.hs`

This submission compiles `lib/TwoPartyEscrow.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.64.0.0 line.

Production line with Plinth 1.64.0.0 (no BuiltinCasing). Plugin pragmas live in `plinth-cape-submissions.cabal`; validator modules carry no Plinth-specific options.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 63eaf4b8cb94e6d5be4350fcc481152dca94b15f
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

The produced UPLC writes to `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.64.0.0_Unisay_plain/two_party_escrow.uplc` and matches the `two_party_escrow.uplc` in this submission.

> Retained as the `plain` variant: this was the default implementation for the Plinth 1.64.0.0 line until the monadic decoder became the default (lower fee, size, CPU, and memory).
