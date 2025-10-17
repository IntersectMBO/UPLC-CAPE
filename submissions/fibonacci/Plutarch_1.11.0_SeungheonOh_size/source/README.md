# Plutarch Fibonacci Size-Optimized Implementation

**Source Code**: [exe/fibonacci-size/Main.hs](https://github.com/Unisay/plutarch-cape-submissions/blob/50d90c9e2a0b995f0a76842b01312951e0ea09e2/exe/fibonacci-size/Main.hs)

**Repository**: <https://github.com/Unisay/plutarch-cape-submissions>

**Commit**: `50d90c9e2a0b995f0a76842b01312951e0ea09e2`

**Path**: `exe/fibonacci-size/Main.hs`

This submission uses Plutarch compiler version 1.11.0 (commit 427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6) with size-optimized implementation using custom combinators.

## Optimization Strategy

This implementation focuses on minimizing script size by:

- **Custom `pfix'`**: Inlines the recursion point instead of abstracting it
- **Custom `pif''`**: Avoids hoisting the IfThenElse builtin to reduce overhead

These optimizations reduce the total script size at the cost of slightly higher execution budgets.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/plutarch-cape-submissions
   cd plutarch-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 50d90c9e2a0b995f0a76842b01312951e0ea09e2
   ```

3. Enter the Nix development environment:

   ```bash
   nix develop
   ```

4. Build and run the executable:

   ```bash
   cabal build fibonacci-size
   cabal run fibonacci-size
   ```

5. The compiled UPLC output should match `fibonacci.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
