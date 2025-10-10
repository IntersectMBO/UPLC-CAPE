# Factorial Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Factorial benchmark is a synthetic computational scenario designed to measure the performance characteristics of iterative and recursive algorithms implemented as UPLC programs. This open optimization scenario allows complete freedom in implementation approach, enabling compilers to showcase their best capabilities across multiple test cases including edge cases and moderate computational loads.

## Exact Task

Implement a Factorial function and compile it as a **parameterized UPLC program** that accepts integer inputs and computes factorials correctly across all test cases.

### Core Requirements

1. **Function Implementation**: Create a function that computes factorials using any method:
   - Naive recursion: `factorial(n) = n * factorial(n-1)`
   - Tail-recursive with accumulator
   - Iterative approach with loops
   - Any other approach that correctly computes factorials

2. **Edge Case Handling**: Must properly handle edge cases:
   - `factorial(n) = 1` for n ≤ 0 (handles negative inputs and zero)
   - Correct behavior for all test cases in `scenarios/factorial/cape-tests.json`

3. **Parameterized Program**: The UPLC program must accept integer parameters at runtime and compute results dynamically, not have values hardcoded.

4. **Test Coverage**: Must pass all test cases including:
   - Edge cases: factorial(0), factorial(1), factorial(-5)
   - Small values: factorial(2), factorial(3), factorial(4), factorial(5)
   - Moderate values: factorial(8), factorial(10), factorial(12)

### Implementation Freedom

You have complete freedom to:

- Choose any algorithmic approach (recursive, tail-recursive, iterative, etc.)
- Use metaprogramming and code generation techniques
- Apply loop unrolling, constant folding, or compiler-specific optimizations
- Leverage your compiler's unique strengths and capabilities
- Submit multiple variants with different optimization strategies (use variant suffixes: `_iterative`, `_tail_recursive`, `_pfix`, etc.)

**Goal**: Demonstrate the best achievable performance for factorial computation using your compiler.

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits for all test cases
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results across multiple executions

---
