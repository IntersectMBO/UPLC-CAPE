# fibonacci Plinth 1.67.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `yura/plutus-1.67`

**Commit**: `bf320db5115145f8d8da18bb771fe769dbc4346d`

**Path**: `lib/FibonacciIterative.hs`

This submission compiles `lib/FibonacciIterative.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.67.0.0 line. There is no casing build flag to pass: 1.67 removed the plugin's `datatypes=BuiltinCasing` option and the default `SumsOfProducts` enables casing on its own, so the source repo's `preview` flag and its PREVIEW CPP define are gone and one build produces one artifact per scenario. The program needs protocol version 11 (van Rossem) to be accepted on-chain.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout bf320db5115145f8d8da18bb771fe769dbc4346d
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

The produced UPLC writes to `$CAPE_REPO/submissions/fibonacci/Plinth_1.67.0.0_Unisay/fibonacci.uplc` and matches the UPLC in this submission. This is the first Plinth line whose generator writes straight to the directory CAPE publishes, because there is no longer a casing variant to name apart.

The dev shell pins GHC 9.6.7 but asks for `cabal = "latest"`, so the cabal-install version you get depends on when the flake inputs were locked; at this commit it resolves to 3.18.1.0. The plutus packages resolve to 1.67.0.0 regardless.
