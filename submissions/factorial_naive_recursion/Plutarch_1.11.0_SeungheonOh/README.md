# Factorial Naive Recursion - Plutarch

This submission implements the prescribed naive recursive factorial function using Plutarch 1.11.0.

## Implementation Details

The factorial function follows the exact algorithm specified in the scenario:

```haskell
factorial n
  | n <= 0    = 1
  | otherwise = n * factorial (n - 1)
```

Implementation uses:

- `pfix` for recursion implementation
- `pif` for conditional logic to handle edge cases
- Handles negative inputs and zero by returning 1
- Recursive calculation for positive integers: n! = n \* (n-1)!

## Algorithm Compliance

This implementation strictly follows the prescribed naive recursive algorithm from the `factorial_naive_recursion` scenario specification:

- Direct recursive implementation matching the mathematical definition
- No tail-call optimization or accumulator patterns
- No iterative loops
- No algorithmic optimizations beyond Plutarch's automatic optimizations

## Source Files

- `source/Main.hs` - Plutarch implementation
- `source/README.md` - Reproducibility instructions
- `factorial_naive_recursion.uplc` - Compiled UPLC program

## Compilation

The program is compiled using Plutarch's `compile` function targeting Plutus Core 1.1.0.
