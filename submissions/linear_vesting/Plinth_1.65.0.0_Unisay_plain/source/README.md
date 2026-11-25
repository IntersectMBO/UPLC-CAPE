# linear_vesting Plinth 1.65.0.0 (plain variant) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.65`

**Commit**: `a67fabd5390f69fbf2397281e084ddab7eb12fbd`

**Path**: `lib/LinearVesting/AsData.hs`

This submission compiles `lib/LinearVesting/AsData.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line.

Production line with Plinth 1.65.0.0 (no BuiltinCasing). Plugin pragmas live in `plinth-cape-submissions.cabal`; validator modules carry no Plinth-specific options.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout a67fabd5390f69fbf2397281e084ddab7eb12fbd
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout; the generator stops if it is unset. You can also set it in `.envrc.local`, which the branch's `.envrc` loads.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/linear_vesting/Plinth_1.65.0.0_Unisay_plain/linear_vesting.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `linear_vesting.uplc` in this submission.

> Retained as the `plain` variant: this was the default implementation for the Plinth 1.65.0.0 line until the monadic decoder became the default (lower fee, size, CPU, and memory).
