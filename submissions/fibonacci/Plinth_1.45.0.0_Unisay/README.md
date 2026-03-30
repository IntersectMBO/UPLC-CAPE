# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Plinth_1.45.0.0_Unisay` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: `Plinth 1.45.0.0`
- **Implementation Approach**: `iterative (accumulator-based)`
- **Algorithm Complexity**: O(n) time, O(1) space
- **Compilation Flags**: Standard PlutusTx optimization flags targeting Plutus Core 1.1.0

## Algorithm Description

This implementation uses an iterative approach with accumulators instead of naive recursion:

- Maintains two variables (`prev`, `curr`) representing consecutive Fibonacci numbers
- Loops from 0 to n, updating accumulators in each iteration
- Avoids exponential recalculation of values (O(2^n) → O(n))
- Achieves ~1000x performance improvement over naive recursive approach

The iterative pattern uses tail recursion with a helper function `fibonacciLoop` that:

1. Takes accumulators (prev=0, curr=1) and counter (i=2)
2. Recursively computes: `fibonacciLoop curr (prev+curr) (i+1) n`
3. Terminates when `i > n`, returning `curr`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements
- Expected: ~200-300M CPU units total (vs 213B for naive recursion)
- For fib(25): ~20-70M CPU units (vs 194B for naive recursion)

## Reproducibility

- **Source Available**: true
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Source Location**: `source/FibonacciIterative.hs`
- **Build Tool**: Cabal via plinth-submissions-app
- **Compilation Command**: `cabal run plinth-submissions`

## References

- Naive recursion baseline: 213.5B CPU total
