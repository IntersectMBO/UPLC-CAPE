# htlc Plinth 1.65.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `4a70c059be150822ab3ee56ce8820fc3db81f35b`

**Path**: `lib/HTLC.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

HTLC validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named`. `QualifiedDo` keeps two `do` blocks side by side: `V.do` sequences decode-or-abort stages, `N.do` builds Named walk regions. `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so each `ScriptContext`/datum/`TxInfo` field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`).

This submission is re-optimised for CAPE metrics schema 2.0.0, whose aggregates cover only the happy-path (accept) measurements. The decisive change is per-path fields-regions over `HTLCDatum`: the claim path reads `recipient`/`secretHash`/`timeout`, the refund path reads `payer`/`timeout`, replacing the lazy asData field selectors and shrinking the term. The inliner budget was re-swept against the happy-path-only `total_fee`: the previous optimum `inline-unconditional-growth=52`, tuned under the old summed accept+reject aggregation, no longer wins, and the new optimum is `32` (plateau 32-42). Net effect: `total_fee` 34840 to 28376 lovelace (-18.6%) and script size 1135 to 693 bytes, with `cpu_max`/`mem_max` up under 1.2% as the lexicographic price of the fee optimum.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 4a70c059be150822ab3ee56ce8820fc3db81f35b
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay/htlc.uplc`.
