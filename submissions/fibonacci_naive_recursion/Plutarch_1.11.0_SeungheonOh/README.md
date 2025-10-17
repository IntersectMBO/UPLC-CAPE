# Fibonacci Naive Recursion - Plutarch

This submission implements the prescribed naive recursive Fibonacci function using Plutarch 1.11.0.

## Implementation Details

The Fibonacci function follows the exact algorithm specified in the scenario:

```haskell
fibonacci n
  | n <= 1    = n
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
```

Implementation uses:

- `pfix` for recursion implementation
- `pif` for conditional logic to handle base cases
- Returns n for n ≤ 1 (base cases: fib(0) = 0, fib(1) = 1)
- Recursive calculation for n > 1: fib(n) = fib(n-1) + fib(n-2)

## Algorithm Compliance

This implementation strictly follows the prescribed naive recursive algorithm from the `fibonacci_naive_recursion` scenario specification:

- Direct recursive implementation matching the mathematical definition
- No tail-call optimization or accumulator patterns
- No iterative loops or memoization
- No algorithmic optimizations beyond Plutarch's automatic optimizations

## Source Files

- `source/Main.hs` - Plutarch implementation
- `source/README.md` - Reproducibility instructions
- `fibonacci_naive_recursion.uplc` - Compiled UPLC program

## Compilation

The program is compiled using Plutarch's `compile` function targeting Plutus Core 1.1.0.
