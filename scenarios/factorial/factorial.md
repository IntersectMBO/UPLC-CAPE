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

## Performance Context

**Why comprehensive test cases?**

- **Edge Case Coverage**: Tests factorial(0), factorial(1), and negative inputs to verify correct implementation
- **Scalability Analysis**: Tests range from factorial(2) to factorial(12) to measure performance scaling
- **Budget Safety**: All test cases fit within CEK machine limits for all approaches
- **Implementation Validation**: Catches common bugs like infinite recursion on negative inputs
- **Compiler Comparison**: Shows differences in optimization across different input ranges

**Test Case Characteristics**:

- **factorial(0) to factorial(5)**: Minimal computation, tests base case handling
- **factorial(8) to factorial(12)**: Moderate computation, shows scaling behavior
- **factorial(-5)**: Critical test for negative input handling (common source of infinite recursion)

**Expected Performance Characteristics** (approximate):

- **Naive Recursive**: Higher CPU usage due to function call overhead, scales with input value
- **Tail-Recursive**: Moderate CPU usage, can be optimized to iterative by compiler
- **Iterative**: Lower CPU usage, predictable memory patterns, better scaling
- **Negative inputs**: Should be O(1) with proper implementation (n ≤ 0 → return 1)

---

_This benchmark enables compiler authors to showcase optimization capabilities across edge cases and demonstrate ecosystem innovation for factorial computation._
