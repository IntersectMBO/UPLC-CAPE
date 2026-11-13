# two_party_escrow Plinth 1.64.0.0 (monadic, promoted to default) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64-escrow-thread-token`

**Commit**: `7844aef5541c7bfafba535c16a5ee2edd267b552`

**Path**: `lib/TwoPartyEscrow/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Revision 2 of the scenario (thread token): one script that runs as a minting policy for the token and as a spending validator for the funds, and reads its own hash from the script context. Written in `do`-notation on `Plinth.Validator`, an early-termination monad, together with the typed decoding DSL `Plinth.Decoder.Named`: each `ScriptContext` field is decoded with a single `Constr` walk, and a value that is only compared is never structurally decoded (`Plinth.Encoded`). The validator finds the escrow output with a module-local unique lookup and reads ADA with `assetAmount`. Inliner budget: `inline-unconditional-growth=28`, kept from revision 1 and not swept again.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 7844aef5541c7bfafba535c16a5ee2edd267b552
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/two_party_escrow/Plinth_1.64.0.0_Unisay_monadic/two_party_escrow.uplc`; in this submission that artifact is committed as the default `Plinth_1.64.0.0_Unisay` (the previous plain implementation is retained as `Plinth_1.64.0.0_Unisay_plain`).
