---
category: fixed
---

# Fibonacci (Naive Recursion) Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Fibonacci Naive Recursion benchmark measures compiler optimization effectiveness for a prescribed recursive algorithm. This scenario requires all submissions to implement the exact naive recursive algorithm specified below, enabling pure compiler-to-compiler comparison where performance differences reflect compiler capabilities rather than algorithmic choices.

## Exact Task

Implement the prescribed naive recursive Fibonacci function and compile it as a UPLC program that accepts an integer input and returns the corresponding Fibonacci number.

### Prescribed Algorithm

All submissions MUST translate this algorithm into their source language, preserving its base case and recursive step:

```haskell
fibonacci :: Integer -> Integer
fibonacci n
  | n <= 1    = n
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
```

**Requirements:**

- Naive recursive implementation matching the mathematical definition above
- No hand-written memoization or dynamic programming
- No hand-written iterative loops, accumulator patterns, or other algorithmic shortcuts
- Direct translation of the specified algorithm into your compiler's source language

These requirements apply to the submitted source. General-purpose automatic compiler optimizations that preserve its input/output behavior are allowed, including unrolling and recursion elimination. General-purpose compiler options may enable these transformations. Record command-line options in `compilation_config.flags`; options set in source pragmas or annotations are covered by the pinned source. The generated UPLC may have a different structure.

### Test Suite

Your implementation must pass all test cases in `cape-tests.json`:

- Edge cases: `fibonacci(0) = 0`, `fibonacci(1) = 1`, `fibonacci(-1) = -1`
- Small values: `fibonacci(2) = 1`, `fibonacci(3) = 2`, `fibonacci(5) = 5`
- Medium values: `fibonacci(8) = 21`, `fibonacci(10) = 55`, `fibonacci(15) = 610`
- Larger values: `fibonacci(20) = 6765`, `fibonacci(25) = 75025`

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results across multiple executions

## Algorithm Compliance

Submissions are reviewed during PR process to verify algorithm compliance:

- Submission README must explain how the implementation matches the prescribed algorithm
- Source code and compilation instructions are required, either inline or through a pinned external source reference as described in the [Submission Guide](../../doc/submission-guide.md#source-code-reproducibility)
- Reviewers verify that, for every input, the source selects the same algorithmic branch and makes the same recursive calls with the same arguments as the definition above
- Additional hand-written base cases such as `n == 2` are deviations even though results are identical
- Compiler-specific syntax and idioms, including either order of the two recursive calls, are acceptable as long as the algorithm structure is preserved

---

_This benchmark enables compiler authors to demonstrate their optimization capabilities on a standardized algorithm, providing valuable insights for the Cardano developer community._
