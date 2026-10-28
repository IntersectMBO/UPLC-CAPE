# Scalus Fibonacci (Naive Recursion) Implementation

**Source Code**: [FibonacciNaiveRecursion.scala](https://github.com/Unisay/scalus-cape-submissions/blob/8d6a62c9fee015de6a8d97308846023dc6cf6792/src/fibonacci_naive_recursion/FibonacciNaiveRecursion.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Branch**: `scalus-0.18.2`

**Commit**: `8d6a62c9fee015de6a8d97308846023dc6cf6792`

**Path**: `src/fibonacci_naive_recursion/FibonacciNaiveRecursion.scala`

This submission uses Scalus compiler version 0.18.2. Fibonacci program computing `fib(n)` by naive (exponential) recursion (`Data -> Unit`), compiled with Scalus default options (no optimization). Targets the van Rossem protocol version (Cardano protocol version 11), live on mainnet since 2026-07-18. Same source as the pre-fork Scalus_0.18.2_Unisay submission it replaces, recompiled with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`, which enables `case-on-builtins` and batch-6 builtins (e.g. `dropList`). Measured on `PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)`.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 8d6a62c9fee015de6a8d97308846023dc6cf6792
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain fibonacci_naive_recursion.compileFibonacciNaiveRecursion'
   ```

   The `@main` writes both the changPV artifact (`fibonacci_naive_recursion`) and the `-preview.uplc` into `src/fibonacci_naive_recursion/`; this submission pins the `-preview.uplc` build.
