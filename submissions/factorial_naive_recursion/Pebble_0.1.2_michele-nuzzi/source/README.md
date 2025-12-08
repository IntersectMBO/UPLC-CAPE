# Pebble Factorial Naive Recursion Implementation

**Source Code**: [factorial_naive_recursion.pebble](https://github.com/Unisay/pebble-cape-submissions/blob/ebb35dbe7538a0e95006e0b91dfff1eec022f1ca/benchmarks/factorial_naive_recursion/factorial_naive_recursion.pebble)

**Repository**: <https://github.com/Unisay/pebble-cape-submissions>

**Commit**: `ebb35dbe7538a0e95006e0b91dfff1eec022f1ca`

**Path**: `benchmarks/factorial_naive_recursion/factorial_naive_recursion.pebble`

This submission uses Pebble compiler version 0.1.2 with naive recursive implementation.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/pebble-cape-submissions
   cd pebble-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout ebb35dbe7538a0e95006e0b91dfff1eec022f1ca
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
