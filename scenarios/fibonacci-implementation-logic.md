# Fibonacci Implementation Logic View

This document provides detailed implementation specifications and pseudocode for the Fibonacci algorithm benchmark.

> **Main Specification**: See [fibonacci.md](./fibonacci.md) for the complete task description, requirements, and submission guidelines. This document provides detailed algorithmic guidance for implementers who need comprehensive implementation references.

## Implementation Philosophy

**Compiler Author's Choice**: The framework places no requirements or expectations on which implementation approach to use. Compiler authors are free to choose the approach that best demonstrates their compiler's strengths and optimization capabilities.

**Function vs Application**: The implementation describes a generic Fibonacci function that accepts an argument. For benchmarking, this function is fully-applied with a specific value (the application is what gets measured, not the function definition itself).

## Core Implementation Specification

### Primary Recursive Implementation

The most straightforward implementation uses simple recursion to match the mathematical definition of Fibonacci numbers:

```pseudocode
// Generic Fibonacci function that accepts an argument
function fibonacci_recursive(n):
    // Base cases
    if n == 1:
        return 1
    if n == 2:
        return 1

    // Recursive case
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
```

### Implementation Requirements

1. **Function Parameter**: Must accept an integer argument `n`
2. **Base Cases**: Both `fibonacci(1)` and `fibonacci(2)` return 1
3. **Mathematical Correctness**: Must implement the correct Fibonacci sequence
4. **Deterministic Output**: Must produce consistent results for the same input

## Alternative Implementation Approaches

**Note**: These are provided for reference only. Compiler authors may choose any approach or create their own variations.

### Iterative Implementation

For compilers that excel at loop optimization:

```pseudocode
// Generic iterative Fibonacci function
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

## Framework Requirements

### Functional Requirements

- **Function Parameter**: Must accept an integer argument `n`
- **Mathematical Correctness**: Must implement the correct Fibonacci sequence
- **No External Dependencies**: Cannot rely on external libraries or oracles
- **Pure Function**: No side effects or state mutations (memoization within function scope is acceptable)
- **Deterministic Output**: Must produce consistent results for the same input

### Performance Characteristics

The framework measures performance when the function is applied to a specific argument:

- **CPU Usage**: Total execution units consumed during computation
- **Memory Usage**: Peak memory allocation during computation
- **Script Efficiency**: Compiled UPLC script size and optimization

### Compilation and Execution Requirements

- **Function Definition**: Implement as a function that accepts an argument
- **Full Application**: For benchmarking, the function is fully-applied with a specific value
- **Self-Contained**: Complete function that can be executed independently
- **Budget Compliant**: Must execute within CEK machine execution limits when applied

## Verification Points

### Correctness Verification

1. **Base Case 1**: `fibonacci(1) = 1`
2. **Base Case 2**: `fibonacci(2) = 1`
3. **Small Values**: `fibonacci(3) = 2`, `fibonacci(4) = 3`, `fibonacci(5) = 5`
4. **Mathematical Sequence**: Must follow the correct Fibonacci sequence for any input

### Performance Verification

1. **CPU Budget Compliance**: Must execute within CEK machine limits
2. **Memory Budget Compliance**: Must not exceed memory allocation limits
3. **Script Size**: Should be reasonably compact for the computation performed
4. **Execution Consistency**: Should produce identical results across multiple runs

## Compiler-Specific Guidance

### Implementation Strategy

**Choose What Works Best**: Select the implementation approach that:

- Best demonstrates your compiler's optimization capabilities
- Aligns with your language's paradigms and strengths
- Produces the most efficient UPLC output for your use case

## Common Implementation Considerations

### Potential Challenges

1. **Base Case Handling**: Ensure correct handling of `fibonacci(1)` and `fibonacci(2)`
2. **Sequence Indexing**: Verify correct Fibonacci sequence numbering
3. **Integer Arithmetic**: Handle integer operations efficiently in UPLC
4. **Execution Budget**: Ensure implementation stays within CEK machine limits
5. **Compile-Time Integration**: Properly embed the target value during compilation

### Success Metrics

The framework measures success based on:

- **Correctness**: Producing the mathematically correct result
- **Efficiency**: Demonstrating good resource utilization
- **Completeness**: Successfully executing within budget constraints

**No Implementation Preference**: The framework treats all correct implementations equally, regardless of the algorithmic approach chosen.

## Cross-References

- **Main Specification**: [fibonacci.md](./fibonacci.md) - Complete task description and requirements
- **Behavioral Testing**: [fibonacci-behavioral-scenarios.md](./fibonacci-behavioral-scenarios.md) - Gherkin test scenarios
- **State Management**: [fibonacci-state-machine.md](./fibonacci-state-machine.md) - Execution flow diagrams
