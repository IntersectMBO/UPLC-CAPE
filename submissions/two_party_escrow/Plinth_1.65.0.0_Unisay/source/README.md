# two_party_escrow Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `yura/escrow-30min-165`

**Commit**: `593f3953df06a0087b0b07ddf3549b2350401eb9`

**Path**: `lib/TwoPartyEscrow.hs` (+ `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Encoded.hs`)

The monadic two-party escrow validator with builtin casing plus the `dropList` decoder step. Both have been mainnet features since the van Rossem hard fork (protocol version 11, 2026-07-18).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 593f3953df06a0087b0b07ddf3549b2350401eb9
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator with the casing build flag (the source repo still gates it behind `preview` at this commit):

```bash
nix develop
cabal run --flags=preview plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.65.0.0_Unisay_preview/two_party_escrow.uplc`, because the generator at this commit still names the casing output `_preview`. That file is this submission's artifact, moved into this directory when the preview track was retired, and it matches the UPLC here.
