---
category: fixed
---

# Factorial (Naive Recursion) Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Factorial Naive Recursion benchmark measures compiler optimization effectiveness for a prescribed recursive algorithm. This scenario requires all submissions to implement the exact naive recursive algorithm specified below, enabling pure compiler-to-compiler comparison where performance differences reflect compiler capabilities rather than algorithmic choices.

## Exact Task

Implement the prescribed naive recursive Factorial function and compile it as a **parameterized UPLC program** that accepts integer inputs and computes factorials across all test cases.

### Prescribed Algorithm

All submissions MUST translate this algorithm into their source language, preserving its base case and recursive step:

```haskell
factorial :: Integer -> Integer
factorial n
  | n <= 0    = 1
  | otherwise = n * factorial (n - 1)
```

**Requirements:**

- Naive recursive implementation matching the mathematical definition above
- Proper edge case handling (n ≤ 0 returns 1)
- No hand-written tail-recursive or accumulator-based rewrites
- No hand-written iterative loops or other algorithmic shortcuts
- Direct translation of the specified algorithm into your compiler's source language

These requirements apply to the submitted source. General-purpose automatic compiler optimizations that preserve its input/output behavior are allowed, including base-case simplification and recursion elimination. General-purpose compiler options may enable these transformations. Record command-line options in `compilation_config.flags`; options set in source pragmas or annotations are covered by the pinned source. The generated UPLC may have a different structure.

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
- Source code and compilation instructions are required, either inline or through a pinned external source reference as described in the [Submission Guide](../../doc/submission-guide.md#source-code-reproducibility)
- Reviewers verify that, for every input, the source selects the same algorithmic branch and makes the same recursive calls with the same arguments as the definition above
- Conditions equivalent over all integer inputs, such as `n < 1` for `n <= 0`, are allowed; stopping at `n <= 1` is a deviation even though results are identical
- Compiler-specific syntax and idioms are acceptable as long as the algorithm structure is preserved

---

_This benchmark enables compiler authors to demonstrate their optimization capabilities on a standardized algorithm, providing valuable insights for the Cardano developer community._
