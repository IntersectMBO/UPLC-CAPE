# two_party_escrow Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.65-escrow-30min`

**Commit**: `1de61b3a7bda3a0f6a0617e0e99c0877a379728d`

**Path**: `lib/TwoPartyEscrow.hs` (+ `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Encoded.hs`)

The two-party escrow validator (revision 2, thread token) on the Named decoder, with builtin casing plus the `dropList` decoder step. Both have been mainnet features since the van Rossem hard fork (protocol version 11, 2026-07-18).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 1de61b3a7bda3a0f6a0617e0e99c0877a379728d
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
