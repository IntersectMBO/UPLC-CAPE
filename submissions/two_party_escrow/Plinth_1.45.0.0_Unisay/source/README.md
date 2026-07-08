# two_party_escrow Plinth 1.45.0.0 (monadic, promoted to default) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `b10eb9c4be0745a199c1febe963e00631ca0c3d7`

**Path**: `lib/TwoPartyEscrow/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Two-party escrow validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named`. `QualifiedDo` keeps `V.do` (decode-or-abort stages) beside `N.do` (Named walk regions); `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so each `ScriptContext`/datum field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`). Retains the escrow security hardening: the escrow input is tied to the script's own payment credential, and the incomplete-withdrawal guard compares the payment credential only (ignoring the staking part). Re-optimised for CAPE metrics schema 2.0.0 (happy-path aggregates only): total_fee 373950 to 147952 lovelace (-60.4%), promoted to the default for this line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b10eb9c4be0745a199c1febe963e00631ca0c3d7
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.45.0.0_Unisay_monadic/two_party_escrow.uplc`; in this submission that artifact is committed as the default `Plinth_1.45.0.0_Unisay` (the previous plain implementation is retained as `Plinth_1.45.0.0_Unisay_plain`).
