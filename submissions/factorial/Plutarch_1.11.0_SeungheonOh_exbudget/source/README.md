# Plutarch Factorial Execution Budget-Optimized Implementation

**Source Code**: [exe/factorial-exbudget/Main.hs](https://github.com/Unisay/plutarch-cape-submissions/blob/994cc9eff4a63e62509ba6b25cedafcbe4680824/exe/factorial-exbudget/Main.hs)

**Repository**: <https://github.com/Unisay/plutarch-cape-submissions>

**Commit**: `994cc9eff4a63e62509ba6b25cedafcbe4680824`

**Path**: `exe/factorial-exbudget/Main.hs`

This submission uses Plutarch compiler version 1.11.0 (commit 427e49b0cbfd2ff97cdc91522a09e06edd7a7fa6) with execution budget-optimized implementation.

## Optimization Strategy

This implementation focuses on minimizing CPU and memory costs by:

- **Custom `pfix'`**: Inlines the recursion point TWICE (duplicates function body)
  - Reduces indirection overhead during execution
  - Results in lower CPU and memory consumption
  - Increases script size due to code duplication

- **Custom `pif''`**: Avoids hoisting the IfThenElse builtin to reduce execution overhead

These optimizations minimize execution budgets at the cost of larger script size.

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
   cabal build factorial-exbudget
   cabal run factorial-exbudget
   ```

5. The compiled UPLC output should match `factorial_exbudget.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
