# Plutarch Fibonacci Naive Recursion Implementation

**Source Code**: [exe/fibonacci-naive/Main.hs](https://github.com/Unisay/plutarch-cape-submissions/blob/994cc9eff4a63e62509ba6b25cedafcbe4680824/exe/fibonacci-naive/Main.hs)

**Repository**: <https://github.com/Unisay/plutarch-cape-submissions>

**Commit**: `994cc9eff4a63e62509ba6b25cedafcbe4680824`

**Path**: `exe/fibonacci-naive/Main.hs`

This submission uses Plutarch compiler version 1.11.0 (commit 427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6) with naive recursive implementation using `pfix` for recursion.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/plutarch-cape-submissions
   cd plutarch-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 994cc9eff4a63e62509ba6b25cedafcbe4680824
   ```

3. Enter the Nix development environment:

   ```bash
   nix develop
   ```

4. Build and run the executable:

   ```bash
   cabal build fibonacci-naive
   cabal run fibonacci-naive
   ```

5. The compiled UPLC output should match `fibonacci_naive_recursion.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
