# htlc Plinth 1.71.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.71`

**Commit**: `bcb8d73ba0e19b9582e62a3c2ace1b7167a74eb8`

**Path**: `lib/HTLC.hs` (fixture: `lib/HTLC/Fixture.hs`; decoder DSL: `lib/Plinth/Validator.hs`, `lib/Plinth/Encoded.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Decoder/Named/TH.hs`)

This submission compiles `lib/HTLC.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.71.0.0 line. 1.71 moved the plugin's default `target-version` from 1.1.0 to 1.2.0, which mainnet does not accept. The source repo sets `target-version=1.1.0` explicitly, so the program header stays `1.1.0` and the program needs protocol version 11 (van Rossem) to be accepted on-chain.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout bcb8d73ba0e19b9582e62a3c2ace1b7167a74eb8
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout; the build aborts if the variable is unset. The recommended place is `.envrc.local` (gitignored), e.g.:

```sh
export CAPE_REPO="$HOME/src/UPLC-CAPE"
```

Then enter the dev shell and run the generator. One invocation, no flags:

```bash
nix develop
cabal run plinth-submissions
```

The produced UPLC writes to `$CAPE_REPO/submissions/htlc/Plinth_1.71.0.0_Unisay/htlc.uplc` and matches the UPLC in this submission.

The dev shell pins GHC 9.6.7 but asks for `cabal = "latest"`, so the cabal-install version you get depends on when the flake inputs were locked; at this commit it resolves to 3.18.1.0. The plutus packages resolve to 1.71.0.0 regardless.
