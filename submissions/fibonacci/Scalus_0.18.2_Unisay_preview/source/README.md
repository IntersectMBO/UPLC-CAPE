# Scalus Fibonacci (Prepacked) Implementation

**Source Code**: [FibonacciPrepacked.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5/src/fibonacci_prepacked/FibonacciPrepacked.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5`

**Path**: `src/fibonacci_prepacked/FibonacciPrepacked.scala`

This submission uses Scalus compiler version 0.18.2. Prepacked Fibonacci implementation: pre-computed `fib(0)`..`fib(25)` packed as 3-byte big-endian entries in a `ByteString`, looked up in O(1) constant time (`Data -> Unit`). Preview submission targeting the van Rossem hard fork (Cardano protocol version 11). Same source as the corresponding current-track Scalus_0.18.2_Unisay submission, recompiled with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`, which enables `case-on-builtins` and batch-6 builtins (e.g. `dropList`). Measured on `PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)`. Invalid on mainnet until the hard fork activates; routed to the preview report via `min_plutus_version = 1.60.0.0`.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain fibonacci_prepacked.compileFibonacciPrepacked'
   ```

   The `@main` writes both the changPV artifact (`fibonacci_prepacked`) and the `-preview.uplc` into `src/fibonacci_prepacked/`; this submission pins the `-preview.uplc` build.
