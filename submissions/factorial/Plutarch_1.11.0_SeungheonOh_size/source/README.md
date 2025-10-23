# Plutarch Factorial Size-Optimized Implementation

**Source Code**: [exe/factorial-size/Main.hs](https://github.com/Unisay/plutarch-cape-submissions/blob/994cc9eff4a63e62509ba6b25cedafcbe4680824/exe/factorial-size/Main.hs)

**Repository**: <https://github.com/Unisay/plutarch-cape-submissions>

**Commit**: `994cc9eff4a63e62509ba6b25cedafcbe4680824`

**Path**: `exe/factorial-size/Main.hs`

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
   git checkout 994cc9eff4a63e62509ba6b25cedafcbe4680824
   ```

3. Enter the Nix development environment:

   ```bash
   nix develop
   ```

4. Build and run the executable:

   ```bash
   cabal build factorial-size
   cabal run factorial-size
   ```

5. The compiled UPLC output should match `factorial_size.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
