# ECD (Euclidean Common Divisor) Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The ECD (Euclidean Common Divisor, also known as GCD - Greatest Common Divisor) benchmark measures compiler optimization effectiveness for a prescribed recursive algorithm. This scenario requires all submissions to implement the exact naive recursive Euclidean algorithm specified below, enabling pure compiler-to-compiler comparison where performance differences reflect compiler capabilities rather than algorithmic choices.

## Exact Task

Implement the prescribed naive recursive Euclidean algorithm and compile it as a **parameterized UPLC program** that accepts pairs of integer inputs and computes their greatest common divisor across all test cases.

### Prescribed Algorithm

All submissions MUST implement this exact algorithm:

```haskell
ecd :: Integer -> Integer -> Integer
ecd a b
  | b == 0    = abs a
  | otherwise = ecd b (a `mod` b)
```

**Requirements:**

- Naive recursive implementation matching the classical Euclidean algorithm above
- Proper edge case handling (when b = 0, return abs a)
- No tail-call optimization or accumulator patterns
- No iterative loops
- No algorithmic optimizations beyond compiler's automatic optimizations
- Direct translation of the specified algorithm into your compiler's source language

### Test Suite

Your implementation must pass all test cases in `cape-tests.json`:

- Edge cases: `ecd(0, 12) = 12`, `ecd(12, 0) = 12`, `ecd(n, n) = n`
- Small values: `ecd(6, 9) = 3`, `ecd(12, 8) = 4`, `ecd(15, 25) = 5`
- Coprime numbers: `ecd(17, 19) = 1`, `ecd(13, 29) = 1`
- Moderate values: `ecd(48, 18) = 6`, `ecd(100, 75) = 25`
- Larger values: `ecd(1071, 462) = 21`, `ecd(2520, 1890) = 630`
- Negative inputs: `ecd(-12, 8) = 4`, `ecd(12, -8) = 4`

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
- No deviations from the specified algorithm are permitted (e.g., no tail-recursion, no iterative loops, no Stein's algorithm)
- Compiler-specific syntax and idioms are acceptable as long as the algorithm structure is preserved

---

_This benchmark enables compiler authors to demonstrate their optimization capabilities on a standardized algorithm, providing valuable insights for the Cardano developer community._
