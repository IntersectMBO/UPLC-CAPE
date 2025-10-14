# Fibonacci Naive Recursion

**Scenario:** fibonacci_naive_recursion

## Implementation

Naive recursive implementation using Plutarch's standard library `pfix` combinator:

```haskell
pfiboNaive :: Term s (PInteger :--> PInteger)
pfiboNaive =
  pfix #$ plam $ \self n ->
    pif (n #<= 1) n (self # (n - 1) + self # (n - 2))
```

## Characteristics

- **Algorithm:** Simple double recursive (exponential time complexity, no optimization)
- **Fixed-point combinator:** Standard library `pfix` from `Plutarch.Internal.Fix`
- **Edge case:** fibonacci(n) = n for n ≤ 1

## Build

```bash
cd plutarch-cape-submissions
cabal run fibonacci-naive
```

## Output

`fibonacci_naive_recursion.uplc`
