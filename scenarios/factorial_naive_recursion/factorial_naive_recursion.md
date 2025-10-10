# Factorial (Naive Recursion) Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Factorial Naive Recursion benchmark measures compiler optimization effectiveness for a prescribed recursive algorithm. This scenario requires all submissions to implement the exact naive recursive algorithm specified below, enabling pure compiler-to-compiler comparison where performance differences reflect compiler capabilities rather than algorithmic choices.

## Exact Task

Implement the prescribed naive recursive Factorial function and compile it as a **parameterized UPLC program** that accepts integer inputs and computes factorials across all test cases.

### Prescribed Algorithm

All submissions MUST implement this exact algorithm:

```haskell
factorial :: Integer -> Integer
factorial n
  | n <= 0    = 1
  | otherwise = n * factorial (n - 1)
```

**Requirements:**

- Naive recursive implementation matching the mathematical definition above
- Proper edge case handling (n ≤ 0 returns 1)
- No tail-call optimization or accumulator patterns
- No iterative loops
- No algorithmic optimizations beyond compiler's automatic optimizations
- Direct translation of the specified algorithm into your compiler's source language

### Test Suite

Your implementation must pass all test cases in `cape-tests.json`:

- Edge cases: `factorial(0) = 1`, `factorial(1) = 1`, `factorial(-5) = 1`
- Small values: `factorial(2) = 2`, `factorial(3) = 6`, `factorial(4) = 24`, `factorial(5) = 120`
- Moderate values: `factorial(8) = 40320`, `factorial(10) = 3628800`, `factorial(12) = 479001600`

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits for all test cases
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results across multiple executions

## Algorithm Compliance

Submissions are reviewed during PR process to verify algorithm compliance:

- Submission README must explain how the implementation matches the prescribed algorithm
- Reviewers verify source code (if available) follows the naive recursive pattern
- No deviations from the specified algorithm are permitted (e.g., no tail-recursion, no iterative loops)
- Compiler-specific syntax and idioms are acceptable as long as the algorithm structure is preserved

---

_This benchmark enables compiler authors to demonstrate their optimization capabilities on a standardized algorithm, providing valuable insights for the Cardano developer community._
