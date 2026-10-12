# Aiken Fibonacci source

**Author**: KtorZ.

**Repository**: <https://github.com/Unisay/aiken-cape-submissions>

**Commit**: `6ed3866d07d9dd100973ae612f3fd29dd5d6f8a8`

**Source**: [benchmarks/fibonacci_prepacked/lib/fibonacci.ak](https://github.com/Unisay/aiken-cape-submissions/blob/6ed3866d07d9dd100973ae612f3fd29dd5d6f8a8/benchmarks/fibonacci_prepacked/lib/fibonacci.ak)

**Compiler**: Aiken 1.1.24.

## Reproduction

```sh
git clone https://github.com/Unisay/aiken-cape-submissions
cd aiken-cape-submissions
git checkout 6ed3866d07d9dd100973ae612f3fd29dd5d6f8a8
nix develop --command build-aiken
```

The result is `out/fibonacci_prepacked/fibonacci.uplc`. Set `CAPE_REPO` and run `nix develop --command build-aiken --install` to copy all three artifacts into CAPE. Run `treefmt` in CAPE to normalize textual UPLC formatting. The build script removes exactly the exported integer argument's `unIData` conversion; it preserves the original builtin-Integer benchmark interface.
