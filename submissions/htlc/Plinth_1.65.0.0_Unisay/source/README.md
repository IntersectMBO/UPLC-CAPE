# htlc Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `6b75b5a3fe1cf56b195cc4222cf939a2812cee78`

**Path**: `lib/HTLC.hs` (decoder DSL: `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`)

This submission compiles `lib/HTLC.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line and the `datatypes=BuiltinCasing` plugin flag. This build also emits the batch-6 `dropList` builtin for cursor gaps of three or more fields in the typed decoder: a single `dropList` call replaces the chained `tailList` steps and its term size does not grow with the gap. `dropList` is only accepted from the van Rossem protocol version onward, so the emission was gated behind the same build flag; the non-casing sibling for this line keeps the pure `tailList` induction. Requires `plutus-core >= 1.65.0.0`.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 6b75b5a3fe1cf56b195cc4222cf939a2812cee78
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

The produced UPLC writes to `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay/htlc.uplc` and matches the UPLC in this submission.
