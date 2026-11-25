# fibonacci_naive_recursion Plinth 1.64.0.0 (BuiltinCasing) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64`

**Commit**: `1999e3dc700416dce562dd2b841700ccdaa4424c`

**Path**: `lib/Fibonacci.hs`

This submission compiles `lib/Fibonacci.hs` from the Plinth source repository with the Plinth (plutus-tx-plugin) 1.64.0.0 line and the `datatypes=BuiltinCasing` plugin flag. Builtin casing has been a mainnet feature since the van Rossem hard fork (protocol version 11, 2026-07-18).

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
cabal run --flags=builtincasing plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/fibonacci_naive_recursion/Plinth_1.64.0.0_Unisay/fibonacci_naive_recursion.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `fibonacci_naive_recursion.uplc` in this submission.
