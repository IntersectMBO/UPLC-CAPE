# Factorial Benchmark Scenario

## Overview

The Factorial benchmark is a **synthetic computational scenario** designed to measure the performance characteristics of iterative and recursive algorithms implemented as UPLC programs. This benchmark tests a compiler's ability to optimize mathematical computations, manage stack operations, and handle integer arithmetic efficiently across multiple input values and edge cases.

## Evaluation Modes

This scenario supports two evaluation modes to serve different benchmarking needs:

### Base Mode (Compiler Comparison)

**Purpose**: Pure compiler-to-compiler comparison with fixed algorithm

All "base" mode submissions MUST implement the prescribed naive recursive algorithm below:

```haskell
factorial :: Integer -> Integer
factorial n
  | n <= 0    = 1
  | otherwise = n * factorial (n - 1)
```

**Requirements:**

- Naive recursive implementation matching the mathematical definition
- Proper edge case handling (n ≤ 0 returns 1)
- No tail-call optimization or accumulator patterns
- No iterative loops
- No algorithmic optimizations beyond compiler's automatic optimizations

**Directory naming**: `submissions/factorial/{Compiler}_{Version}_{Author}_base/`

**Use case**: Comparing how different compilers optimize the same recursive algorithm

### Open Mode (Real-World Competition)

**Purpose**: Showcase compiler ecosystem capabilities with any optimization technique

**Allowed**:

- Any algorithmic approach (recursive, iterative, tail-recursive, accumulator-based)
- Loop unrolling, constant folding, or any compiler-specific optimizations
- Metaprogramming and code generation techniques
- Multiple submissions per compiler/author using unique slugs

**Directory naming**:

- Generic implementation: `submissions/factorial/{Compiler}_{Version}_{Author}_open/`
- Specific optimization: `submissions/factorial/{Compiler}_{Version}_{Author}_open_{slug}/`

**Slug examples** (optional): `tail-recursive`, `iterative`, `unrolled`, `pfix`

**Use case**: Demonstrating best achievable performance for factorial computation

### Mode Selection

When creating a submission:

```bash
# Base mode (prescribed naive recursive algorithm)
cape submission new factorial MyCompiler 1.0.0 handle --mode base

# Open mode (generic/default optimization)
cape submission new factorial MyCompiler 1.0.0 handle --mode open

# Open mode (specific optimization with slug)
cape submission new factorial MyCompiler 1.0.0 handle --mode open --slug tail-recursive
```

Reports show both modes by default, or filter with `--mode base` or `--mode open`.

## TL;DR

Implement a Factorial function that passes **comprehensive test cases** ranging from edge cases to moderate computational loads, compiled as a parameterized UPLC program.

**Required Files**: Submit `factorial.uplc`, `metadata.json`, `metrics.json` to `submissions/factorial/{Compiler}_{Version}_{Handle}_{mode}[_{slug}]/`

**Test Cases**: 10 comprehensive test cases including factorial(0), factorial(10), and negative inputs **Metrics**: Per-test CPU/memory units, aggregated measurements, script size, term size **Constraints**: Plutus Core 1.1.0, Plutus V3 recommended, CEK machine budget limits **Implementation**: Must handle negative inputs correctly (return 1 for n ≤ 0)

---

## Exact Task

Implement a Factorial function and compile it as a **parameterized UPLC program** that accepts integer inputs and computes factorials correctly across all test cases.

### Core Requirements

1. **Function Implementation**: Create a function that computes factorials using the mathematical definition with proper edge case handling:
   - `factorial(n) = 1` for n ≤ 0 (handles negative inputs and zero)
   - `factorial(n) = n * factorial(n-1)` for n > 0

2. **Parameterized Program**: The UPLC program must accept integer parameters at runtime and compute results dynamically.

3. **Comprehensive Testing**: Must pass all test cases in `scenarios/factorial/cape-tests.json` including:
   - Edge cases: factorial(0), factorial(1)
   - Small values: factorial(2), factorial(3), factorial(4), factorial(5)
   - Moderate values: factorial(8), factorial(10), factorial(12)
   - Negative inputs: factorial(-5)

### Implementation Approaches

Choose the approach that works best for your compiler:

**Note**: You have full freedom in implementation approach - recursive, iterative, tail-recursive, or any other method that correctly handles all test cases. The critical requirement is proper handling of edge cases, particularly negative inputs (n ≤ 0 should return 1).

---

## Acceptance Criteria

Your submission passes if:

- ✅ **Comprehensive Correctness**: Passes all 10 test cases in `scenarios/factorial/cape-tests.json`
- ✅ **Negative Input Handling**: Correctly returns 1 for factorial(-5) and any n ≤ 0
- ✅ **Budget Compliance**: All test cases execute within CEK machine CPU and memory limits
- ✅ **Determinism**: Produces identical results across multiple executions
- ✅ **Parameterized**: Accepts integer inputs at runtime (not hardcoded values)
- ✅ **File Format**: Valid UPLC program that can be executed by the CEK evaluator

---

## Metrics Recorded

All submissions are measured on these standardized metrics:

| Metric | Description | Purpose |
| --- | --- | --- |
| **CPU Units** | Total execution units consumed | Computational efficiency |
| **Memory Units** | Peak memory usage during execution | Memory efficiency |
| **Script Size** | Compiled UPLC script size in bytes | Code generation efficiency |
| **Term Size** | UPLC term representation size | Optimization effectiveness |

**Measurement Environment**: Standard CEK machine evaluator with default budget limits.

### Performance Context

**Why comprehensive test cases?**

- **Edge Case Coverage**: Tests factorial(0), factorial(1), and negative inputs to verify correct implementation
- **Scalability Analysis**: Tests range from factorial(2) to factorial(12) to measure performance scaling
- **Budget Safety**: All test cases fit within CEK machine limits for both recursive and iterative approaches
- **Implementation Validation**: Catches common bugs like infinite recursion on negative inputs
- **Compiler Comparison**: Shows differences in optimization across different input ranges

**Test Case Performance Characteristics**:

- **factorial(0) to factorial(5)**: Minimal computation, tests base case handling
- **factorial(8) to factorial(12)**: Moderate computation, shows scaling behavior
- **factorial(-5)**: Critical test for negative input handling (common source of infinite recursion)

**Expected Performance Ranges** (approximate):

- **Recursive**: Higher CPU usage due to function call overhead, scales with input value
- **Iterative**: Lower CPU usage, more predictable memory patterns, better scaling
- **Negative inputs**: Should be O(1) with proper implementation (n ≤ 0 → return 1)

---

## Submission Checklist

Before submitting your implementation:

- [ ] **Verify Result**: Program produces exactly `3628800` when executed
- [ ] **Test Budget**: Execution completes without budget exhaustion
- [ ] **Prepare Files**:
  - [ ] `factorial.uplc` - Your compiled UPLC program
  - [ ] `metadata.json` - Compiler info, optimization settings, implementation notes
  - [ ] `metrics.json` - Performance measurements (CPU, memory, script size, term size)
  - [ ] `README.md` - Brief description of your approach
- [ ] **Directory Structure**: Place in `submissions/factorial/{Compiler}_{Version}_{Handle}/`
- [ ] **Schema Validation**: Ensure JSON files match required schemas

### File Templates

Use these templates from `submissions/TEMPLATE/`:

- `metadata-template.json` for compiler and build information
- `metrics-template.json` for performance measurements

---

## Local Validation

1. **Functional Test**: Execute your UPLC program and verify output is `3628800`
2. **Budget Test**: Ensure execution completes within CEK machine limits
3. **Consistency Test**: Run multiple times to confirm deterministic behavior
4. **Schema Test**: Validate JSON files against schemas in `submissions/TEMPLATE/`

### Example Validation Commands

```bash
# Measure your UPLC program (if using the cape tool)
cape submission measure .

# Verify correctness and validate submission files
cape submission verify submissions/factorial/YourCompiler_1.0.0_YourHandle/
```

See USAGE.md at the project root for the full CLI reference.

---

## Technical Constraints

- **Plutus Core Version**: Target Plutus Core 1.1.0
- **Plutus Version**: V3 recommended (V1, V2 acceptable)
- **Budget Limits**: Must complete within standard CEK machine execution limits
- **No External Dependencies**: Program must be self-contained
- **Deterministic**: Must produce consistent results

---

## Verification Points

### Correctness Verification

1. **Base Case**: `factorial(0) = 1`
2. **Small Values**: `factorial(1) = 1`, `factorial(2) = 2`, `factorial(3) = 6`, `factorial(4) = 24`
3. **Target Value**: `factorial(10) = 3628800`

### Performance Verification

1. **CPU Budget Compliance**: Must execute within CEK machine limits
2. **Memory Budget Compliance**: Must not exceed memory allocation limits
3. **Script Size**: Should be reasonably compact for the computation performed
4. **Execution Consistency**: Should produce identical results across multiple runs

---

_This benchmark serves as both a correctness test and performance comparison tool, enabling compiler authors to validate their mathematical computation handling while providing standardized metrics for community comparison._
