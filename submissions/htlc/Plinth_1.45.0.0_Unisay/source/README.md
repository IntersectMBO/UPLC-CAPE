# htlc Plinth 1.45.0.0 (monadic variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `49c94429c49b7c9e7d038e5685ba85e07b9d332b`

**Path**: `lib/HTLC/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

HTLC monadic validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named`, on the frozen `plinth-1.45` branch. `QualifiedDo` keeps two `do` blocks side by side: `V.do` sequences decode-or-abort stages, `N.do` builds Named walk regions. `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so each `ScriptContext`/datum/`TxInfo` field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`).

This submission is re-optimised for CAPE metrics schema 2.0.0, whose aggregates cover only the happy-path (accept) measurements. The decisive change is per-path fields-regions over `HTLCDatum`: the claim path reads `recipient`/`secretHash`/`timeout`, the refund path reads `payer`/`timeout`, replacing the lazy asData field selectors. `inline-unconditional-growth` is a no-op on the 1.45 plugin (default/24/32/52/200 build byte-identically), so the pragma is removed and the whole gain is structural. Net effect: `total_fee` 48023 to 44476 lovelace (-7.4%), with every axis improved (`cpu_max` -12.7%, `mem_max` -15.0%, script size 1254 to 1242 bytes).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 49c94429c49b7c9e7d038e5685ba85e07b9d332b
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.45.0.0_Unisay_monadic/htlc.uplc` (and regenerates the sibling `Plinth_1.45.0.0_Unisay` artifact unchanged, since HTLC.Monadic keeps its own compile splice and doesn't touch `HTLC.hs`).
