# Scalus Fibonacci (Prepacked) Implementation

**Source Code**: [FibonacciPrepacked.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5/src/fibonacci_prepacked/FibonacciPrepacked.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5`

**Path**: `src/fibonacci_prepacked/FibonacciPrepacked.scala`

This submission uses Scalus compiler version 0.18.2. Prepacked Fibonacci implementation: pre-computed `fib(0)`..`fib(25)` packed as 3-byte big-endian entries in a `ByteString`, looked up in O(1) constant time (`Data -> Unit`). Compiled with `Options.release`.

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
