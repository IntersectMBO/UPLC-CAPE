# htlc Plinth 1.64.0.0 (monadic variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64`

**Commit**: `1999e3dc700416dce562dd2b841700ccdaa4424c`

**Path**: `lib/HTLC/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

HTLC monadic validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named`, on the frozen `plinth-1.64` branch. `QualifiedDo` keeps two `do` blocks side by side: `V.do` sequences decode-or-abort stages, `N.do` builds Named walk regions. `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so each `ScriptContext`/datum/`TxInfo` field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`).

This submission is re-optimised for CAPE metrics schema 2.0.0, whose aggregates cover only the happy-path (accept) measurements. The decisive change is per-path fields-regions over `HTLCDatum`: the claim path reads `recipient`/`secretHash`/`timeout`, the refund path reads `payer`/`timeout`, replacing the lazy asData field selectors and shrinking the term. The inliner budget was re-swept against the happy-path-only `total_fee`: the previous optimum `inline-unconditional-growth=52` no longer wins, and the new optimum is `24` (plateau 24-30). Net effect: `total_fee` 38776 to 31826 lovelace (-17.9%) and script size 1227 to 736 bytes, with `cpu_max`/`mem_max` up under 2.4% as the lexicographic price of the fee optimum.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 1999e3dc700416dce562dd2b841700ccdaa4424c
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout; the generator stops if it is unset. You can also set it in `.envrc.local`, which the branch's `.envrc` loads.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.64.0.0_Unisay/htlc.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `htlc.uplc` in this submission.
