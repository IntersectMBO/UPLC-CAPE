# fibonacci_naive_recursion Plinth 1.65.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.65`

**Commit**: `a67fabd5390f69fbf2397281e084ddab7eb12fbd`

**Path**: `lib/Fibonacci.hs`

This submission compiles `lib/Fibonacci.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.65.0.0 line and the `datatypes=BuiltinCasing` plugin flag. Builtin casing has been a mainnet feature since the van Rossem hard fork (protocol version 11, 2026-07-18).

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
cabal run --flags=builtincasing plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/fibonacci_naive_recursion/Plinth_1.65.0.0_Unisay/fibonacci_naive_recursion.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `fibonacci_naive_recursion.uplc` in this submission.
