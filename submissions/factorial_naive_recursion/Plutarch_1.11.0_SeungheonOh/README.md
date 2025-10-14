# Factorial Naive Recursion

**Scenario:** factorial_naive_recursion

## Implementation

Naive recursive implementation using Plutarch's standard library `pfix` combinator:

```haskell
pfactorialNaive :: Term s (PInteger :--> PInteger)
pfactorialNaive =
  pfix #$ plam $ \self n ->
    pif (n #<= 0) 1 (n * (self # (n - 1)))
```

## Characteristics

- **Algorithm:** Simple recursive (linear time complexity, no optimization)
- **Fixed-point combinator:** Standard library `pfix` from `Plutarch.Internal.Fix`
- **Edge case:** factorial(n) = 1 for n ≤ 0

## Build

```bash
cd plutarch-cape-submissions
cabal run factorial-naive
```

## Output

`factorial_naive_recursion.uplc`
