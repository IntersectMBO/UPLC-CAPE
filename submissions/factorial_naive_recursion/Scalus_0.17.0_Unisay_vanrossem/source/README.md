# Scalus Factorial Naive Recursion Implementation (vanRossem preview)

**Source Code**: [FactorialNaiveRecursion.scala](https://github.com/Unisay/scalus-cape-submissions/blob/becb41d62cb2832001de1541ff2dcde0d3ac92c6/src/factorial_naive_recursion/FactorialNaiveRecursion.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `becb41d62cb2832001de1541ff2dcde0d3ac92c6`

**Path**: `src/factorial_naive_recursion/FactorialNaiveRecursion.scala`

The Scala source is identical to the current-track `Scalus_0.17.0_Unisay/` submission. The difference is in the compile command: this preview artifact is produced with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`, which enables `case-on-builtins` and batch-6 builtins (e.g. `dropList`). The same `@main factorial_naive_recursion.compileFactorialNaiveRecursion` writes both the current-track and the vanRossem artifact in one run.

The output is invalid on mainnet until the van Rossem hard fork (Cardano protocol version 11) activates — projected late-June 2026.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout becb41d62cb2832001de1541ff2dcde0d3ac92c6
   ```

3. Run the @main object:

   ```bash
   sbt 'runMain factorial_naive_recursion.compileFactorialNaiveRecursion'
   ```

4. The compiled UPLC output should match `factorial.uplc` in this submission (it is written to `src/factorial_naive_recursion/factorial-vanrossem.uplc` upstream).

For detailed build instructions and environment setup, see the repository README.
