# two_party_escrow Plinth 1.64.0.0 (monadic, promoted to default) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64`

**Commit**: `1999e3dc700416dce562dd2b841700ccdaa4424c`

**Path**: `lib/TwoPartyEscrow/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Revision 2 of the scenario (thread token): one script that runs as a minting policy for the token and as a spending validator for the funds, and reads its own hash from the script context. Written in `do`-notation on `Plinth.Validator`, an early-termination monad, together with the typed decoding DSL `Plinth.Decoder.Named`: each `ScriptContext` field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`). The validator finds the escrow output with a module-local unique lookup and reads ADA with `assetAmount`. Inliner budget: `inline-unconditional-growth=28`, kept from revision 1 and not swept again.

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

The generator writes `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.64.0.0_Unisay/two_party_escrow.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `two_party_escrow.uplc` in this submission.
