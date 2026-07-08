# two_party_escrow Plinth 1.65.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `f9ef6bdf4ed08ec16039600903d8af7e6c22046b`

**Path**: `lib/TwoPartyEscrow.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Two-party escrow validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named`. `QualifiedDo` keeps `V.do` (decode-or-abort stages) beside `N.do` (Named walk regions); `IxDecoder` tracks the walk cursor in the type via a Peano-indexed fundep class (`FieldAt`), so each `ScriptContext`/datum field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`). Retains the escrow security hardening: the escrow input is tied to the script's own payment credential, and the incomplete-withdrawal guard compares the payment credential only (ignoring the staking part). Re-optimised for CAPE metrics schema 2.0.0 (happy-path aggregates only): total_fee 311093 to 101588 lovelace (-67.3%), promoted to the default for this line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout f9ef6bdf4ed08ec16039600903d8af7e6c22046b
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.65.0.0_Unisay/two_party_escrow.uplc` (monadic is the default on `main`; the previous asdata implementation is retained as `Plinth_1.65.0.0_Unisay_asdata`).
