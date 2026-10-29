# Pebble Fibonacci Naive Recursion Implementation

**Source Code**: [fibonacci_naive_recursion.pebble](https://github.com/Unisay/pebble-cape-submissions/blob/b13c02e811f2e71670740da22abfb9f092c720c3/benchmarks/fibonacci_naive_recursion/fibonacci_naive_recursion.pebble)

**Repository**: <https://github.com/Unisay/pebble-cape-submissions>

**Branch**: `pebble-0.5.0`

**Commit**: `b13c02e811f2e71670740da22abfb9f092c720c3`

**Path**: `benchmarks/fibonacci_naive_recursion/fibonacci_naive_recursion.pebble`

This submission uses Pebble compiler version 0.5.0 (`@harmoniclabs/pebble-cli` pinned to exactly `0.5.0` in `package.json`) with naive recursive implementation. The `.pebble` source and the compiled UPLC are byte-identical to the 0.4.4 submission.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/pebble-cape-submissions
   cd pebble-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout b13c02e811f2e71670740da22abfb9f092c720c3
   ```

3. Enter the Nix development environment:

   ```bash
   nix develop
   ```

4. Install dependencies:

   ```bash
   bun install
   ```

5. Build the benchmark:

   ```bash
   bun run compile:fib
   ```

6. The compiled UPLC output should match `fibonacci_naive_recursion.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
