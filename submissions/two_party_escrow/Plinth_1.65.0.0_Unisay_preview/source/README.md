# two_party_escrow Plinth 1.65.0.0 (BuiltinCasing + dropList) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `d078652f03d31ed728c1fb63f1d9f8824218494c`

**Path**: `lib/TwoPartyEscrow.hs` (+ `lib/Plinth/Decoder.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Encoded.hs`)

The preview build of the monadic two-party escrow validator: BuiltinCasing plus the dropList decoder step, requiring `plutus-core >= 1.65.0.0` (not yet on mainnet).

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout d078652f03d31ed728c1fb63f1d9f8824218494c
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generatorTrue:

```bash
nix develop
cabal run --flags=preview plinth-submissions
```
