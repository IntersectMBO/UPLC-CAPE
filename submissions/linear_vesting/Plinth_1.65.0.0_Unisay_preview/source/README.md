# linear_vesting Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `6b75b5a3fe1cf56b195cc4222cf939a2812cee78`

**Path**: `lib/LinearVesting.hs` (decoder DSL: `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`)

This submission compiles `lib/LinearVesting.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line and the BuiltinCasing preview flag enabled. The preview build also emits the batch-6 `dropList` builtin for cursor gaps of three or more fields in the typed decoder: a single `dropList` call replaces the chained `tailList` steps and its term size does not grow with the gap. `dropList` is only accepted from the van Rossem protocol version, so the emission is gated to the preview build (the production build keeps the pure `tailList` induction and stays byte-identical). Requires `plutus-core >= 1.65.0.0` (not yet on mainnet).

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

Then enter the dev shell and run the generator with the preview flag:

```bash
nix develop
cabal run --flags=preview plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/linear_vesting/Plinth_1.65.0.0_Unisay_preview/linear_vesting.uplc` and matches the UPLC in this submission.
