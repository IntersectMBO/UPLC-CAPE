# Factorial Implementation - Plutarch

This submission implements the factorial function using Plutarch 1.11.1.

## Implementation Details

The factorial function uses:

- `pfix'` for recursion implementation
- Conditional logic with `pif''` to handle edge cases
- Handles negative inputs and zero by returning 1
- Recursive calculation for positive integers: n! = n \* (n-1)!

## Source Files

- `source/factorial.hs` - Main Plutarch implementation
- `factorial.uplc` - Compiled UPLC program

## Compilation

The program is compiled using Plutarch's `compile` function with Plutus Core 1.1.0 target.
