# Factorial Base Mode

**Mode:** Base (naive recursive)

## Implementation

Naive recursive implementation matching the mathematical definition:

```scala
def factorial(n: BigInt): BigInt =
  if n <= 0 then BigInt(1)
  else n * factorial(n - 1)
```

## Characteristics

- **Algorithm:** Simple recursive (linear time complexity, but no tail-call optimization)
- **Optimization:** None (as prescribed by base mode)
- **Edge case:** factorial(n) = 1 for n ≤ 0

## Build

```bash
sbt "runMain factorial.compileFactorialBase"
```

## Output

`submissions/factorial/base/factorial.uplc`
