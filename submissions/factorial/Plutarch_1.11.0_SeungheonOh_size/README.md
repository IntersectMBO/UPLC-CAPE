# Factorial (Size-Optimized) - Plutarch

This submission implements a size-optimized factorial function using Plutarch 1.11.0.

## Implementation Details

The factorial function uses custom combinators optimized for minimal script size:

- **`pfix'`**: Custom fixpoint combinator that inlines the recursion point
  - Reduces abstraction overhead
  - Results in smaller UPLC script size

- **`pif''`**: Custom conditional that doesn't hoist the IfThenElse builtin
  - Avoids creating additional lambda abstractions
  - Further reduces script size

- Handles edge cases (n ≤ 0) by returning 1
- Recursive calculation for positive integers: n! = n \* (n-1)!

## Optimization Trade-offs

This size-optimized variant prioritizes:

- ✅ Minimal script size
- ⚠️ Slightly higher CPU/memory costs compared to exbudget-optimized variant

## Source Files

- `source/Main.hs` - Plutarch implementation with custom combinators
- `source/README.md` - Reproducibility instructions
- `factorial_size.uplc` - Compiled UPLC program

## Compilation

The program is compiled using Plutarch's `compile` function targeting Plutus Core 1.1.0.
