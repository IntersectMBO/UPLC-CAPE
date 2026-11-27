# htlc Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.65`

**Commit**: `a67fabd5390f69fbf2397281e084ddab7eb12fbd`

**Path**: `lib/HTLC.hs` (decoder DSL: `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`)

This submission compiles `lib/HTLC.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line and the `datatypes=BuiltinCasing` plugin flag. This build also emits the batch-6 `dropList` builtin for cursor gaps of three or more fields in the typed decoder: a single `dropList` call replaces the chained `tailList` steps and its term size does not grow with the gap. `dropList` is only accepted from the van Rossem protocol version onward, so the emission was gated behind the same build flag; the non-casing sibling for this line keeps the pure `tailList` induction. Requires `plutus-core >= 1.65.0.0`.

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

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay/htlc.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `htlc.uplc` in this submission.
