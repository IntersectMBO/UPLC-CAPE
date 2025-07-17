# Fibonacci Benchmark Scenario

## Overview

The Fibonacci benchmark is a **synthetic computational scenario** designed to measure the performance characteristics of recursive algorithms implemented as UPLC programs. This benchmark tests a compiler's ability to optimize recursive function calls, manage stack depth, and handle integer arithmetic operations efficiently.

**Purpose**: This scenario serves as a standardized test for measuring:
- CPU unit consumption for recursive computations
- Memory unit usage during stack-intensive operations  
- Script size optimization for mathematical algorithms
- Term size efficiency in recursive function encoding

**Key Constraint**: The UPLC program must be **fully-applied** before benchmarking, meaning the Fibonacci number to compute is baked into the script itself rather than passed as a parameter. This ensures consistent measurement across all compiler implementations and prevents variations based on different argument values.

**Target Computation**: `fibonacci(25) = 75025`
- This value is chosen to be computationally significant enough to measure performance differences
- It fits comfortably within the CEK machine execution budget limits
- It provides sufficient recursive depth to test optimization capabilities

**Success Criteria**: The program must successfully compute the 25th Fibonacci number (75025) within the execution budget constraints.

---

## View 1: State Lifecycle View

The Fibonacci program operates as a **pure computational function** with a simple execution model:

```
[Start] --execute()--> [Computed Result: 75025]
```

| Current State | Event | Condition | Next State |
|--------------|-------|-----------|------------|
| **Start** | `execute()` | Program starts execution | **Computing** |
| **Computing** | `fibonacci_computation()` | Recursive computation proceeds | **Computing** (until base case) |
| **Computing** | `base_case_reached()` | n <= 2 in recursion | **Result** |
| **Result** | - | Result = 75025 | **Complete** |

**State Descriptions**:
- **Start**: Program begins execution with the hardcoded target (25)
- **Computing**: Recursive computation is in progress
- **Result**: Computation completes with the final result
- **Complete**: Program execution terminates successfully

**Note**: This is a pure computation - there are no failure states since the input is fixed and known to be valid.

---

## View 2: Behavioral Scenario View

### Feature: Fibonacci Computation
To compute the 25th Fibonacci number efficiently while measuring performance characteristics.

#### Scenario: Standard Fibonacci Computation
```gherkin
Given a Fibonacci program is compiled to compute fibonacci(25)
And the program is fully-applied with the target value 25
When the program is executed on the CEK machine
Then the computation should complete successfully
And the result should equal 75025
And the CPU units consumed should be recorded
And the memory units consumed should be recorded
And the execution should complete within budget limits
```

#### Scenario: Execution Budget Compliance
```gherkin
Given a Fibonacci program is compiled to compute fibonacci(25)
And the CEK machine has standard execution budget limits
When the program is executed
Then the execution should not exceed CPU unit limits
And the execution should not exceed memory unit limits
And the computation should complete without budget exhaustion
```

#### Scenario: Deterministic Results
```gherkin
Given a Fibonacci program is compiled to compute fibonacci(25)
When the program is executed multiple times
Then each execution should produce the same result (75025)
And each execution should consume the same amount of CPU units
And each execution should consume the same amount of memory units
```

#### Scenario: Compiler Optimization Comparison
```gherkin
Given two Fibonacci programs compiled with different optimization levels
And both programs compute fibonacci(25)
When both programs are executed
Then both should produce the result 75025
But the optimized version should consume fewer CPU units
And the optimized version should use memory more efficiently
```

---

## View 3: Implementation Logic View

### Pseudocode for the Fibonacci Program

```pseudocode
// Main program - fully applied with target value
function FibonacciProgram():
    // The target value is baked into the program at compile time
    target = 25
    
    // Compute the Fibonacci number
    result = fibonacci_recursive(target)
    
    // Return the result (75025)
    return result

function fibonacci_recursive(n):
    // Base cases
    if n == 1:
        return 1
    if n == 2:
        return 1
    
    // Recursive case
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
```

### Alternative Optimized Implementation

For compilers that support iterative approaches or memoization:

```pseudocode
// Main program - fully applied with target value
function FibonacciProgram():
    // The target value is baked into the program at compile time
    target = 25
    
    // Compute using iterative approach
    result = fibonacci_iterative(target)
    
    // Return the result (75025)
    return result

function fibonacci_iterative(n):
    if n == 1 OR n == 2:
        return 1
    
    prev1 = 1  // fib(1)
    prev2 = 1  // fib(2)
    
    for i in range(3, n + 1):
        current = prev1 + prev2
        prev1 = prev2
        prev2 = current
    
    return prev2
```

### Memoized Implementation (if supported)

```pseudocode
// Main program with memoization
function FibonacciProgram():
    target = 25
    memo = create_empty_map()
    result = fibonacci_memoized(target, memo)
    return result

function fibonacci_memoized(n, memo):
    if n in memo:
        return memo[n]
    
    if n == 1 OR n == 2:
        memo[n] = 1
        return 1
    
    result = fibonacci_memoized(n - 1, memo) + fibonacci_memoized(n - 2, memo)
    memo[n] = result
    return result
```

---

## Benchmark Metrics

### Primary Metrics
- **CPU Units**: Total execution units consumed during computation
- **Memory Units**: Peak memory usage during computation
- **Script Size**: Size of the compiled UPLC script in bytes
- **Term Size**: Size of the UPLC term representation

### Standardized Test Configuration

| Benchmark Name | Target Computation | Expected Result | Purpose |
|----------------|-------------------|----------------|---------|
| `fibonacci_25` | fibonacci(25) | 75025 | Standard benchmark for recursive computation |

**Rationale for fibonacci(25)**:
- **Computationally Significant**: Requires 242,785 recursive calls in naive implementation
- **Budget Compliant**: Fits well within CEK machine execution limits
- **Optimization Sensitive**: Large enough to show meaningful differences between optimized and unoptimized code
- **Deterministic**: Always produces the same result (75025)

### Metadata Requirements

Each benchmark result must include:
- **Compiler**: Name and version (e.g., "Aiken 1.0.8", "Plutus 1.2.0")
- **Optimization Level**: Compiler optimization settings used
- **Implementation Approach**: "recursive" or "iterative" or "memoized"
- **Plutus Version**: Target Plutus version (V1, V2, V3)
- **Target Value**: Always 25 (for consistency verification)
- **Expected Result**: Always 75025 (for correctness verification)
- **Execution Environment**: Local simulation vs actual on-chain execution

---

## Implementation Notes

### For Compiler Authors

1. **Full Application Required**: The program must be compiled with the target value (25) baked in, not parameterized
2. **Choose Your Approach**: Implement recursive, iterative, or memoized version based on your compiler's strengths
3. **Optimization Opportunities**: This benchmark tests recursive function optimization, tail-call optimization, and arithmetic operation efficiency
4. **Budget Awareness**: Ensure the implementation fits within standard CEK machine execution budgets
5. **Deterministic Execution**: The computation must be deterministic across all executions

### Expected Performance Characteristics

- **Recursive Implementation**: Exponential resource usage - approximately O(φ^n) where φ ≈ 1.618
- **Iterative Implementation**: Linear resource usage - O(n)
- **Memoized Implementation**: Linear resource usage with higher memory overhead - O(n)
- **Compiler Optimization**: Significant differences expected between optimization levels

### Budget Considerations

The choice of fibonacci(25) ensures:
- **Recursive**: ~242K function calls, should complete within budget
- **Iterative**: 24 iterations, minimal resource usage
- **Memoized**: 25 memoization entries, moderate memory usage

This benchmark serves as both a correctness test and a performance comparison tool, allowing compiler authors to validate their recursive function handling while providing standardized metrics for community comparison.
