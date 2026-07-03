# htlc Plinth 1.45.0.0 (monadic variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `ead9b62dfafddcff35b654dc20c2a095389da062`

**Path**: `lib/HTLC/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

HTLC validator variant built on `Plinth.Validator`, a zero-cost Cont-style early-termination monad that sequences decode-or-abort steps and boolean guards into a flat, short-circuiting `do`-chain, plus a zero-cost typed decoding DSL (`Plinth.Decoder.Named`), originally added in [Unisay/plinth-cape-submissions#10](https://github.com/Unisay/plinth-cape-submissions/pull/10) for Plinth 1.65.0.0 and ported unchanged onto the frozen `plinth-1.45` branch. `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so walk regions are placed directly between guards and double as the decode plan; `Plinth.Encoded` gives a zero-cost typed view of still-`Data`-encoded values so a field that is only compared is never structurally decoded. Each ScriptContext/datum/TxInfo field is walked at most once. Plugin pragmas live in `plinth-cape-submissions.cabal`; the validator module sets `inline-unconditional-growth=52` (same tuning as the 1.65.0.0 original), which lets the inliner fuse the decode walks — a larger term but lower CPU/mem.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout ead9b62dfafddcff35b654dc20c2a095389da062
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.45.0.0_Unisay_monadic/htlc.uplc` (and regenerates the sibling `Plinth_1.45.0.0_Unisay` artifact unchanged, since HTLC.Monadic keeps its own compile splice and doesn't touch `HTLC.hs`).
