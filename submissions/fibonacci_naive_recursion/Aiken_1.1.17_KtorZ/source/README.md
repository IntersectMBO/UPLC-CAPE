# Aiken Fibonacci source

**Author**: KtorZ.

**Repository**: <https://github.com/Unisay/aiken-cape-submissions>

**Commit**: `0a4a8789b18b1a075cdcf76411190a9d2083b988`

**Source**: [archive/Aiken_1.1.17_KtorZ/source/fib.ak](https://github.com/Unisay/aiken-cape-submissions/blob/0a4a8789b18b1a075cdcf76411190a9d2083b988/archive/Aiken_1.1.17_KtorZ/source/fib.ak)

**Compiler**: Aiken 1.1.17.

## Reproduction

The original source and submitted UPLC are preserved under [`archive/Aiken_1.1.17_KtorZ`](https://github.com/Unisay/aiken-cape-submissions/tree/0a4a8789b18b1a075cdcf76411190a9d2083b988/archive/Aiken_1.1.17_KtorZ). Put `source/fib.ak` in `lib/fibonacci.ak` of an otherwise empty Plutus V3 Aiken project, then run `aiken export --module fibonacci --name fibonacci` with Aiken 1.1.17. Decode `compiledCode` with `aiken uplc decode --cbor --hex`, then remove the exported `unIData` argument conversion to accept builtin integers, as the original contributor did manually. The archive retains the original submitted artifact as the reference result.
