# Factorial (Execution Budget-Optimized) - Plutarch

This submission implements an execution budget-optimized factorial function using Plutarch 1.11.0.

## Implementation Details

The factorial function uses custom combinators optimized for minimal CPU and memory costs:

- **`pfix'`**: Custom fixpoint combinator that inlines the recursion point TWICE
  - Duplicates the function body to reduce indirection overhead
  - Significantly reduces CPU and memory consumption during execution
  - Results in larger UPLC script size due to code duplication

- **`pif''`**: Custom conditional that doesn't hoist the IfThenElse builtin
  - Avoids creating additional lambda abstractions
  - Reduces execution overhead

- Handles edge cases (n ≤ 0) by returning 1
- Recursive calculation for positive integers: n! = n \* (n-1)!

## Optimization Trade-offs

This exbudget-optimized variant prioritizes:

- ✅ Minimal CPU and memory costs
- ⚠️ Larger script size compared to size-optimized variant

The key difference from the size-optimized variant is that this version duplicates the recursion function body (inlines twice) to minimize execution indirection, trading script size for better runtime performance.

## Source Files

- `source/Main.hs` - Plutarch implementation with custom combinators
- `source/README.md` - Reproducibility instructions
- `factorial_exbudget.uplc` - Compiled UPLC program

## Compilation

The program is compiled using Plutarch's `compile` function targeting Plutus Core 1.1.0.
