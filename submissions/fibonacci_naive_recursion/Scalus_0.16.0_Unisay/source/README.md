# Scalus Fibonacci Naive Recursion Implementation

**Source Code**: [FibonacciNaiveRecursion.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6c0871c587cfc491f3d09dc50204ee3def132e06/src/fibonacci_naive_recursion/FibonacciNaiveRecursion.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Branch**: `scalus-0.16.0`

**Commit**: `6c0871c587cfc491f3d09dc50204ee3def132e06`

**Path**: `src/fibonacci_naive_recursion/FibonacciNaiveRecursion.scala`

This submission uses Scalus compiler version 0.16.0 with a naive recursive implementation (no optimization, per scenario specification), compiled with `Options.release`. `removeTraces=true`/`generateErrorTraces=false` have no visible effect on this scenario since the source has no `fail()`/`require()` calls to strip.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 6c0871c587cfc491f3d09dc50204ee3def132e06
   ```

3. Follow build instructions in the repository README

4. The compiled UPLC output should match `fibonacci_naive_recursion.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
