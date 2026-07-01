# htlc Plinth 1.65.0.0 (monadic variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `616fe5da245e8478aec9a1e2fcbac652893f2313`

**Path**: `lib/HTLC/Monadic.hs` (+ `lib/Plinth/Validator.hs`)

HTLC validator variant built on `Plinth.Validator`, a zero-cost Cont-style early-termination monad that sequences decode-or-abort steps and boolean guards into a flat, short-circuiting `do`-chain. Each ScriptContext/datum/TxInfo field is decoded at most once and only when a guard reaches it; a value that is only compared is never structurally decoded. Plugin pragmas live in `plinth-cape-submissions.cabal`; per-module inliner tuning (`inline-unconditional-growth=110`) lives in the validator module header.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 616fe5da245e8478aec9a1e2fcbac652893f2313
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/htlc/Plinth_1.65.0.0_Unisay_monadic/htlc.uplc`, byte-identical to the `htlc.uplc` in this submission (md5 5dd30b1a662f727a978f331d514548a9).
