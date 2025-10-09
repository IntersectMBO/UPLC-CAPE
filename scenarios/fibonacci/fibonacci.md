# Fibonacci Benchmark Scenario

> **For submission requirements, metrics explanation, and validation steps, see the [Submission Guide](../../doc/submission-guide.md)**

## Overview

The Fibonacci benchmark is a synthetic computational scenario designed to measure the performance characteristics of algorithms implemented as UPLC programs. This open optimization scenario allows complete freedom in implementation approach, enabling compilers to showcase their best capabilities including optimization techniques, metaprogramming, and alternative algorithms.

## Exact Task

Implement a Fibonacci function and compile it as a **fully-applied UPLC program** that computes the 25th Fibonacci number.

### Core Requirements

1. **Function Implementation**: Create a function that computes Fibonacci numbers using any method:
   - Naive recursion: `fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)`
   - Iterative approach with accumulators
   - Memoization or dynamic programming
   - Closed-form solution (Binet's formula)
   - Any other approach that correctly computes Fibonacci numbers

2. **Full Application**: The UPLC program must be fully-applied with the target value (25) baked in during compilation, not passed as a parameter. The program should execute immediately to produce the result.

3. **Target Computation**: `fibonacci(25)` must produce exactly `75025`

### Implementation Freedom

You have complete freedom to:

- Choose any algorithmic approach (recursive, iterative, memoized, closed-form, etc.)
- Use metaprogramming and code generation techniques
- Apply loop unrolling, constant folding, or compiler-specific optimizations
- Leverage your compiler's unique strengths and capabilities
- Submit multiple variants with different optimization strategies (use variant suffixes: `_memoized`, `_iterative`, etc.)

**Goal**: Demonstrate the best achievable performance for computing fibonacci(25) using your compiler.

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results across multiple executions

## Performance Context

**Why fibonacci(25)?**

- **Target Result**: Produces exactly 75025
- **Computationally Significant**: ~242,785 recursive calls in naive implementation
- **Budget Safe**: Fits comfortably within CEK machine limits for all approaches
- **Optimization Sensitive**: Large enough to show optimization effectiveness
- **Manageable**: Not so large as to create measurement difficulties

**Expected Performance Characteristics** (approximate):

- **Naive Recursive**: Higher CPU usage, demonstrates compiler's recursion optimization
- **Iterative**: Lower CPU usage, predictable memory patterns
- **Memoized**: Moderate CPU, higher memory usage, demonstrates cache optimization
- **Closed-form**: Very low CPU, demonstrates constant-time computation if feasible

---

_This benchmark enables compiler authors to showcase optimization capabilities and demonstrate ecosystem innovation for Fibonacci computation._
