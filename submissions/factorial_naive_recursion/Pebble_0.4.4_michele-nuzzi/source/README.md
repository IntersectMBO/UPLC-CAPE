# Pebble Factorial Naive Recursion Implementation

**Source Code**: [factorial_naive_recursion.pebble](https://github.com/Unisay/pebble-cape-submissions/blob/f8fad7650d79203983be6f453104e9acc99e33a5/benchmarks/factorial_naive_recursion/factorial_naive_recursion.pebble)

**Repository**: <https://github.com/Unisay/pebble-cape-submissions>

**Branch**: `main`

**Commit**: `f8fad7650d79203983be6f453104e9acc99e33a5`

**Path**: `benchmarks/factorial_naive_recursion/factorial_naive_recursion.pebble`

This submission uses Pebble compiler version 0.4.4 (`@harmoniclabs/pebble-cli` pinned to exactly `0.4.4` in `package.json`) with naive recursive implementation. The `.pebble` source is byte-identical to the 0.1.2 submission.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/pebble-cape-submissions
   cd pebble-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout f8fad7650d79203983be6f453104e9acc99e33a5
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
   bun run compile:fact
   ```

6. The compiled UPLC output should match `factorial_naive_recursion.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.
