# Plutarch Fibonacci Prepacked Implementation

**Source Code**: [exe/fibonacci-prepacked/Main.hs](https://github.com/Unisay/plutarch-cape-submissions/blob/ff41428357febfe4497e7973597535ff06834cd0/exe/fibonacci-prepacked/Main.hs)

**Repository**: <https://github.com/Unisay/plutarch-cape-submissions>

**Commit**: `ff41428357febfe4497e7973597535ff06834cd0`

**Path**: `exe/fibonacci-prepacked/Main.hs`

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
   git checkout ff41428357febfe4497e7973597535ff06834cd0
   ```

3. Enter the Nix development environment:

   ```bash
   nix develop
   ```

4. Build and run the executable:

   ```bash
   cabal build fibonacci-prepacked
   cabal run fibonacci-prepacked
   ```

5. The compiled UPLC output should match `fibonacci_prepacked.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
