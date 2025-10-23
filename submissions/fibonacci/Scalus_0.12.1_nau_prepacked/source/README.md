# Scalus Fibonacci Prepacked Implementation

**Source Code**: [FibonacciPrepacked.scala](https://github.com/Unisay/scalus-cape-submissions/blob/894f7a979a67f0981a1e798df5d42b24dc3c97c9/src/fibonacci_prepacked/FibonacciPrepacked.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `894f7a979a67f0981a1e798df5d42b24dc3c97c9`

**Path**: `src/fibonacci_prepacked/FibonacciPrepacked.scala`

This submission uses Scalus compiler version 0.12.1 with prepacked optimization approach for O(1) constant-time lookup.

## Implementation Approach

- Pre-computed Fibonacci numbers stored in a ByteString
- O(1) constant-time lookup performance
- Pre-computes values for fib(0) through fib(25)
- Each Fibonacci number encoded as 3 bytes in big-endian format
- Optimized with Scalus inlining and case-constr transformations

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 894f7a979a67f0981a1e798df5d42b24dc3c97c9
   ```

3. Run the compilation:

   ```bash
   sbt "runMain fibonacci_prepacked.compileFibonacciPrepacked"
   ```

4. The compiled UPLC output should match `fibonacci.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
