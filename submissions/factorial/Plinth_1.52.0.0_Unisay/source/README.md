# Source Code

The source code for this factorial implementation is located in the main repository:

**[📁 plinth/src/Factorial.hs](../../../../plinth/src/Factorial.hs)**

This Haskell module contains:

- `factorial :: Integer -> Integer` - The factorial function implementation
- `factorial10Code :: CompiledCode Integer` - The compiled UPLC program for factorial(10)

The implementation uses recursive approach: `factorial(n) = if n <= 0 then 1 else n * factorial(n-1)`
