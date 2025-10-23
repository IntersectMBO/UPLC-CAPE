# Plutarch Fibonacci Prepacked Implementation

**Source Code**: [exe/fibonacci-size/Main.hs](https://github.com/Unisay/plutarch-cape-submissions/blob/271222b68f1d398dd1d0f736cf3468f4239d3c1b/exe/fibonacci-prepack/Main.hs)

**Repository**: <https://github.com/Unisay/plutarch-cape-submissions>

**Commit**: `454a9d474cfc6e7936a6b4ee3e1a90a2f2000200`

**Path**: `exe/fibonacci-prepack/Main.hs`

This submission uses Plutarch compiler version 1.11.0 (commit 427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6) with size-optimized implementation using custom combinators.

## Optimization Strategy

This implementation prepacked first 25 fibonacci numbers.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/plutarch-cape-submissions
   cd plutarch-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 454a9d474cfc6e7936a6b4ee3e1a90a2f2000200
   ```

3. Enter the Nix development environment:

   ```bash
   nix develop
   ```

4. Build and run the executable:

   ```bash
   cabal build fibonacci-prepack
   cabal run fibonacci-prepack
   ```

5. The compiled UPLC output should match `fibonacci_prepack.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
