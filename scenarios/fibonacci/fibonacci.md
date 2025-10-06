# Fibonacci Benchmark Scenario

## Overview

The Fibonacci benchmark is a **synthetic computational scenario** designed to measure the performance characteristics of recursive algorithms implemented as UPLC programs. This benchmark tests a compiler's ability to optimize recursive function calls, manage stack depth, and handle integer arithmetic operations efficiently.

## TL;DR

Implement a Fibonacci function that computes **fibonacci(25) = 75025** and compile it as a fully-applied UPLC program.

**Required Files**: Submit `fibonacci.uplc`, `metadata.json`, `metrics.json` to `submissions/fibonacci/{Compiler}_{Version}_{Handle}/`

**Target**: `fibonacci(25)` → Expected result: `75025`  
**Metrics**: CPU units, Memory units, Script size (bytes), Term size  
**Constraints**: Plutus Core 1.1.0, Plutus V3 recommended, CEK machine budget limits  
**Implementation**: Choose recursive, iterative, or memoized approach

---

## Exact Task

Implement a Fibonacci function and compile it as a **fully-applied UPLC program** that computes the 25th Fibonacci number.

### Core Requirements

1. **Function Implementation**: Create a function that computes Fibonacci numbers using the mathematical definition:
   - `fibonacci(1) = 1`
   - `fibonacci(2) = 1`
   - `fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)` for n > 2

2. **Full Application**: The UPLC program must be fully-applied with the target value (25) baked in during compilation, not passed as a parameter.

3. **Target Computation**: `fibonacci(25)` must produce exactly `75025`

### Implementation Approaches

**Note**: You have complete freedom in implementation approach - recursive, iterative, memoized, or any other method that correctly computes Fibonacci numbers. Choose the approach that works best for your compiler's optimization capabilities.

---

## Acceptance Criteria

Your submission passes if:

- ✅ **Correctness**: Program outputs exactly `75025`
- ✅ **Budget Compliance**: Executes within CEK machine CPU and memory limits
- ✅ **Determinism**: Produces identical results across multiple executions
- ✅ **Self-Contained**: No external dependencies or parameters
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

**Why fibonacci(25)?**

- **Computationally Significant**: ~242,785 recursive calls in naive implementation
- **Budget Safe**: Fits comfortably within CEK machine limits
- **Optimization Sensitive**: Large enough to show compiler differences
- **Manageable**: Not so large as to create measurement difficulties

**Expected Performance Ranges** (approximate):

- **Recursive**: Higher CPU usage, demonstrates optimization capabilities
- **Iterative**: Lower CPU usage, more predictable memory patterns
- **Memoized**: Moderate CPU, higher memory usage

---

## Submission Checklist

Before submitting your implementation:

- [ ] **Verify Result**: Program produces exactly `75025` when executed
- [ ] **Test Budget**: Execution completes without budget exhaustion
- [ ] **Prepare Files**:
  - [ ] `fibonacci.uplc` - Your compiled UPLC program
  - [ ] `metadata.json` - Compiler info, optimization settings, implementation notes
  - [ ] `metrics.json` - Performance measurements (CPU, memory, script size, term size)
  - [ ] `README.md` - Brief description of your approach
- [ ] **Directory Structure**: Place in `submissions/fibonacci/{Compiler}_{Version}_{Handle}/`
- [ ] **Schema Validation**: Ensure JSON files match required schemas

### File Templates

Use these templates from `submissions/TEMPLATE/`:

- `metadata-template.json` for compiler and build information
- `metrics-template.json` for performance measurements

---

## Local Validation

1. **Functional Test**: Execute your UPLC program and verify output is `75025`
2. **Budget Test**: Ensure execution completes within CEK machine limits
3. **Consistency Test**: Run multiple times to confirm deterministic behavior
4. **Schema Test**: Validate JSON files against schemas in `submissions/TEMPLATE/`

### Example Validation Commands

```bash
# Measure your UPLC program (if using the cape tool)
cape submission measure .

# Verify correctness and validate submission files
cape submission verify submissions/fibonacci/YourCompiler_1.0.0_YourHandle/
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

1. **Base Cases**: `fibonacci(1) = 1` and `fibonacci(2) = 1`
2. **Small Values**: `fibonacci(3) = 2`, `fibonacci(4) = 3`, `fibonacci(5) = 5`
3. **Target Value**: `fibonacci(25) = 75025`

### Performance Verification

1. **CPU Budget Compliance**: Must execute within CEK machine limits
2. **Memory Budget Compliance**: Must not exceed memory allocation limits
3. **Script Size**: Should be reasonably compact for the computation performed
4. **Execution Consistency**: Should produce identical results across multiple runs

---

_This benchmark serves as both a correctness test and performance comparison tool, enabling compiler authors to validate their recursive function handling while providing standardized metrics for community comparison._
