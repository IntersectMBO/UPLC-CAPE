# two_party_escrow Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.65`

**Commit**: `a67fabd5390f69fbf2397281e084ddab7eb12fbd`

**Path**: `lib/TwoPartyEscrow.hs` (+ `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Encoded.hs`)

The two-party escrow validator (revision 2, thread token) on the Named decoder, with builtin casing plus the `dropList` decoder step. Both have been mainnet features since the van Rossem hard fork (protocol version 11, 2026-07-18).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout a67fabd5390f69fbf2397281e084ddab7eb12fbd
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout; the generator stops if it is unset. You can also set it in `.envrc.local`, which the branch's `.envrc` loads.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run --flags=builtincasing plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.65.0.0_Unisay/two_party_escrow.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `two_party_escrow.uplc` in this submission.
