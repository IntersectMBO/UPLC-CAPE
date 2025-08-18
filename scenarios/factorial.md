# Factorial Benchmark Scenario

The Factorial benchmark is a **synthetic computational scenario** designed to measure the performance characteristics of iterative and recursive algorithms implemented as UPLC programs. This benchmark tests a compiler's ability to optimize mathematical computations, manage stack operations, and handle integer arithmetic efficiently.

## TL;DR

Implement a Factorial function that computes **factorial(10) = 3628800** and compile it as a fully-applied UPLC program.

**Required Files**: Submit `factorial.uplc`, `metadata.json`, `metrics.json` to `submissions/factorial/{Compiler}_{Version}_{Handle}/`

**Target**: `factorial(10)` → Expected result: `3628800`  
**Metrics**: CPU units, Memory units, Script size (bytes), Term size  
**Constraints**: Plutus Core 1.1.0, Plutus V3 recommended, CEK machine budget limits  
**Implementation**: Choose recursive or iterative approach

---

## Exact Task

Implement a Factorial function and compile it as a **fully-applied UPLC program** that computes the factorial of 10.

### Core Requirements

1. **Function Implementation**: Create a function that computes factorials using the mathematical definition:

   - `factorial(0) = 1`
   - `factorial(n) = n * factorial(n-1)` for n > 0

2. **Full Application**: The UPLC program must be fully-applied with the target value (10) baked in during compilation, not passed as a parameter.

3. **Target Computation**: `factorial(10)` must produce exactly `3628800`

### Implementation Approaches

Choose the approach that works best for your compiler:

#### Recursive Implementation (Most Direct)

```pseudocode
function factorial_recursive(n):
    if n == 0:
        return 1
    return n * factorial_recursive(n - 1)
```

#### Iterative Implementation (More Efficient)

```pseudocode
function factorial_iterative(n):
    if n == 0:
        return 1

    result = 1
    for i in range(1, n + 1):
        result = result * i

    return result
```

---

## Acceptance Criteria

Your submission passes if:

- ✅ **Correctness**: Program outputs exactly `3628800`
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

**Why factorial(10)?**

- **Computationally Manageable**: 10! = 3,628,800 fits comfortably in standard integer types
- **Budget Safe**: Fits well within CEK machine limits for both recursive and iterative approaches
- **Optimization Sensitive**: Large enough to show compiler differences in loop vs recursion handling
- **Practical Scale**: Representative of many real-world mathematical computations

**Expected Performance Ranges** (approximate):

- **Recursive**: Higher CPU usage due to function call overhead
- **Iterative**: Lower CPU usage, more predictable memory patterns
- **Tail Recursive**: Moderate CPU usage with optimized stack handling

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
