# Fibonacci Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Fibonacci benchmark is a synthetic computational scenario designed to measure the performance characteristics of algorithms implemented as UPLC programs. This open optimization scenario allows complete freedom in implementation approach, enabling compilers to showcase their best capabilities including optimization techniques, metaprogramming, and alternative algorithms.

## Exact Task

Implement a Fibonacci function and compile it as a UPLC program that accepts an integer input and returns the corresponding Fibonacci number.

### Core Requirements

1. **Function Implementation**: Create a function that computes Fibonacci numbers using any method:
   - Naive recursion: `fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)`
   - Iterative approach with accumulators
   - Memoization or dynamic programming
   - Closed-form solution (Binet's formula)
   - Any other approach that correctly computes Fibonacci numbers

2. **Input Parameter**: The UPLC program must accept a single integer parameter `n` and return `fibonacci(n)`. The program will be tested with multiple input values to ensure correctness across different scenarios.

3. **Test Suite**: Your implementation must pass all test cases in `cape-tests.json`, including:
   - Edge cases: `fibonacci(0) = 0`, `fibonacci(1) = 1`, `fibonacci(-1) = -1`
   - Small values: `fibonacci(2) = 1`, `fibonacci(3) = 2`, `fibonacci(5) = 5`
   - Medium values: `fibonacci(8) = 21`, `fibonacci(10) = 55`, `fibonacci(15) = 610`
   - Larger values: `fibonacci(20) = 6765`, `fibonacci(25) = 75025`

### Implementation Freedom

You have complete freedom to:

- Choose any algorithmic approach (recursive, iterative, memoized, closed-form, etc.)
- Use metaprogramming and code generation techniques
- Apply loop unrolling, constant folding, or compiler-specific optimizations
- Leverage your compiler's unique strengths and capabilities
- Submit multiple variants with different optimization strategies (use variant suffixes: `_memoized`, `_iterative`, etc.)

**Goal**: Demonstrate the best achievable performance for computing Fibonacci numbers across a range of inputs using your compiler.

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results across multiple executions

## Performance Context

**Test Suite Coverage**:

The benchmark tests Fibonacci computation across a range of inputs (0-25 and negative), measuring:

- **Edge case handling**: Base cases (0, 1) and negative inputs
- **Small values**: Quick computation verification (2, 3, 5)
- **Medium values**: Performance characteristics emerge (8, 10, 15)
- **Larger values**: Optimization effectiveness becomes significant (20, 25)

**Why test multiple inputs?**

- **Correctness validation**: Ensures implementation handles edge cases and various input ranges
- **Performance profiling**: Different inputs reveal different optimization characteristics
- **Aggregate metrics**: Sum, maximum, and median metrics provide comprehensive performance picture

**Expected Performance Characteristics**:

- **Naive Recursive**: CPU usage grows exponentially with input; demonstrates compiler's recursion handling
- **Iterative**: Linear CPU growth; predictable memory patterns; efficient for all inputs
- **Memoized**: Near-linear CPU growth; higher memory usage; demonstrates caching effectiveness
- **Closed-form**: Constant-time for all inputs; demonstrates mathematical optimization capability

---

_This benchmark enables compiler authors to showcase optimization capabilities and demonstrate ecosystem innovation for Fibonacci computation._
