# Fibonacci (Naive Recursion) Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Fibonacci Naive Recursion benchmark measures compiler optimization effectiveness for a prescribed recursive algorithm. This scenario requires all submissions to implement the exact naive recursive algorithm specified below, enabling pure compiler-to-compiler comparison where performance differences reflect compiler capabilities rather than algorithmic choices.

## Exact Task

Implement the prescribed naive recursive Fibonacci function and compile it as a **fully-applied UPLC program** that computes the 25th Fibonacci number.

### Prescribed Algorithm

All submissions MUST implement this exact algorithm:

```haskell
fibonacci :: Integer -> Integer
fibonacci n
  | n <= 1    = n
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
```

**Requirements:**

- Naive recursive implementation matching the mathematical definition above
- No memoization or dynamic programming
- No iterative loops or accumulator patterns
- No algorithmic optimizations beyond compiler's automatic optimizations
- Direct translation of the specified algorithm into your compiler's source language

**Why this algorithm?**

- **fibonacci(25) = 75025**: Produces exactly 75025
- **~242,785 recursive calls**: Computationally significant workload
- **Budget safe**: Fits within CEK machine limits
- **Optimization sensitive**: Large enough to show compiler optimization differences

### Full Application

The UPLC program must be fully-applied with the target value (25) baked in during compilation, not passed as a parameter. The program should execute immediately to produce the result.

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results across multiple executions

## Algorithm Compliance

Submissions are reviewed during PR process to verify algorithm compliance:

- Submission README must explain how the implementation matches the prescribed algorithm
- Reviewers verify source code (if available) follows the naive recursive pattern
- No deviations from the specified algorithm are permitted
- Compiler-specific syntax and idioms are acceptable as long as the algorithm structure is preserved

---

_This benchmark enables compiler authors to demonstrate their optimization capabilities on a standardized algorithm, providing valuable insights for the Cardano developer community._
