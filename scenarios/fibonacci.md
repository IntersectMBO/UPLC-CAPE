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

## Detailed Views

This benchmark is documented across multiple specialized views for better organization and readability:

### 📊 [State Machine View](./fibonacci-state-machine.md)

Detailed state lifecycle and transitions with visual Mermaid diagrams showing the program's execution flow from start to completion.

### 🎭 [Behavioral Scenarios](./fibonacci-behavioral-scenarios.md)

Comprehensive Gherkin-based behavioral specifications with test scenarios covering correctness, performance, and edge cases.

### 💻 [Implementation Logic](./fibonacci-implementation-logic.md)

Detailed pseudocode implementations including recursive, iterative, and memoized approaches with compiler-specific guidance.

---

## Benchmark Metrics

### Primary Metrics

- **CPU Units**: Total execution units consumed during computation
- **Memory Units**: Peak memory usage during computation
- **Script Size**: Size of the compiled UPLC script in bytes
- **Term Size**: Size of the UPLC term representation

### Standardized Test Configuration

| Benchmark Name | Target Computation | Expected Result | Purpose |
| --- | --- | --- | --- |
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
