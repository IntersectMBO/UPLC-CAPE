# Scalus Fibonacci Prepacked Implementation

**Source Code**: [FibonacciPrepacked.scala](https://github.com/Unisay/scalus-cape-submissions/blob/2126a807bbe2a744e2b924bda3099881e09d969f/src/fibonacci_prepacked/FibonacciPrepacked.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `2126a807bbe2a744e2b924bda3099881e09d969f`

**Path**: `src/fibonacci_prepacked/FibonacciPrepacked.scala`

This submission uses Scalus compiler version 0.16.0 with a prepacked optimization approach for O(1) constant-time lookup.

## Implementation Approach

- Pre-computed Fibonacci numbers stored in a ByteString
- O(1) constant-time lookup performance
- Pre-computes values for fib(0) through fib(25)
- Each Fibonacci number encoded as 3 bytes in big-endian format

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 2126a807bbe2a744e2b924bda3099881e09d969f
   ```

3. Run the compilation:

   ```bash
   sbt "runMain fibonacci_prepacked.compileFibonacciPrepacked"
   ```

4. The compiled UPLC output should match `fibonacci.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
