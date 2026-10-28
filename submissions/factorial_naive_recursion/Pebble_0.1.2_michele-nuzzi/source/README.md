# Pebble Factorial Naive Recursion Implementation

**Source Code**: [factorial_naive_recursion.pebble](https://github.com/Unisay/pebble-cape-submissions/blob/a906d905aef2c8a58cae8dc4818f65aaf6236f93/benchmarks/factorial_naive_recursion/factorial_naive_recursion.pebble)

**Repository**: <https://github.com/Unisay/pebble-cape-submissions>

**Branch**: `pebble-0.1.2`

**Commit**: `a906d905aef2c8a58cae8dc4818f65aaf6236f93`

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
   git checkout a906d905aef2c8a58cae8dc4818f65aaf6236f93
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
