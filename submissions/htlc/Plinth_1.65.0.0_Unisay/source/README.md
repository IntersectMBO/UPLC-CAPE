# htlc Plinth 1.65.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `83f813ac937870cce30757942b141e2207bfdc34`

**Path**: `lib/HTLC/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

HTLC validator variant built on `Plinth.Validator`, a zero-cost Cont-style early-termination monad that sequences decode-or-abort steps and boolean guards into a flat, short-circuiting `do`-chain, plus a zero-cost typed decoding DSL (`Plinth.Decoder.Named`) added in [Unisay/plinth-cape-submissions#10](https://github.com/Unisay/plinth-cape-submissions/pull/10). `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so walk regions are placed directly between guards and double as the decode plan; `Plinth.Encoded` gives a zero-cost typed view of still-`Data`-encoded values so a field that is only compared is never structurally decoded. Each ScriptContext/datum/TxInfo field is walked at most once. Plugin pragmas live in `plinth-cape-submissions.cabal`; the validator module sets `inline-unconditional-growth=52` (restored in [Unisay/plinth-cape-submissions#11](https://github.com/Unisay/plinth-cape-submissions/pull/11)), which lets the inliner fuse the decode walks — a larger term but lower CPU/mem, worth −17392 lovelace in fee versus the pragma-less build.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 83f813ac937870cce30757942b141e2207bfdc34
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay/htlc.uplc`.
