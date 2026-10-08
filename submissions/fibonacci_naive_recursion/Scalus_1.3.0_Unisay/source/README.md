# Scalus Fibonacci (Naive Recursion) Implementation

**Source Code**: [FibonacciNaiveRecursion.scala](https://github.com/Unisay/scalus-cape-submissions/blob/75678ebd8a1d627102095c4df1b72b774d0ccee8/src/fibonacci_naive_recursion/FibonacciNaiveRecursion.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Branch**: `main`

**Commit**: `75678ebd8a1d627102095c4df1b72b774d0ccee8`

**Path**: `src/fibonacci_naive_recursion/FibonacciNaiveRecursion.scala`

This submission uses Scalus compiler version 1.3.0. Fibonacci program computing `fib(n)` by naive (exponential) recursion (`Data -> Unit`). Targets the van Rossem protocol version (Cardano protocol version 11), live on mainnet since 2026-07-18. Compiled once with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)` (Scala 3.3.8, `scalus-plugin` 1.3.0); there is no separate preview build any more. Compiler changes since 0.18.2 include recursion through self-application instead of the Z combinator, the static-argument transformation, the unified UPLC pipeline, and fee-aware CSE and inlining.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 75678ebd8a1d627102095c4df1b72b774d0ccee8
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain fibonacci_naive_recursion.compileFibonacciNaiveRecursion'
   ```

   The `@main` compiles once and writes `src/fibonacci_naive_recursion/fibonacci.uplc`, which matches the UPLC in this submission.
