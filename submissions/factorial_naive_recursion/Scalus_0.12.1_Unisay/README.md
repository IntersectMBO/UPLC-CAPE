# Factorial Naive Recursion

**Scenario:** factorial_naive_recursion

## Implementation

Naive recursive implementation matching the mathematical definition:

```scala
def factorial(n: BigInt): BigInt =
  if n <= 0 then BigInt(1)
  else n * factorial(n - 1)
```

## Characteristics

- **Algorithm:** Simple recursive (linear time complexity, but no tail-call optimization)
- **Optimization:** None (as prescribed by scenario specification)
- **Edge case:** factorial(n) = 1 for n ≤ 0

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions
