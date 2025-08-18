# Source Code

The source code for this fibonacci implementation is located in the main repository:

**[📁 plinth/src/Fibonacci.hs](../../../../plinth/src/Fibonacci.hs)**

This Haskell module contains:

- `fibonacci :: Integer -> Integer` - The fibonacci function implementation
- `fibonacci25Code :: CompiledCode Integer` - The compiled UPLC program for fibonacci(25)

The implementation uses recursive approach: `fibonacci(n) = if n <= 1 then n else fibonacci(n-1) + fibonacci(n-2)`
