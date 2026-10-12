# Aiken Fibonacci source

**Author**: KtorZ.

**Repository**: <https://github.com/Unisay/aiken-cape-submissions>

**Commit**: `0a4a8789b18b1a075cdcf76411190a9d2083b988`

**Source**: [archive/Aiken_1.1.19_KtorZ_tailrec/source/lib/fibonacci.ak](https://github.com/Unisay/aiken-cape-submissions/blob/0a4a8789b18b1a075cdcf76411190a9d2083b988/archive/Aiken_1.1.19_KtorZ_tailrec/source/lib/fibonacci.ak)

**Compiler**: Aiken 1.1.19.

## Reproduction

The original source, manifests (where provided) and submitted UPLC are preserved under [`archive/Aiken_1.1.19_KtorZ_tailrec`](https://github.com/Unisay/aiken-cape-submissions/tree/0a4a8789b18b1a075cdcf76411190a9d2083b988/archive/Aiken_1.1.19_KtorZ_tailrec). Run `aiken export --module fibonacci --name fibonacci` with Aiken 1.1.19 in the archived `source/` project. Decode `compiledCode` with `aiken uplc decode --cbor --hex`, then remove the exported `unIData` argument conversion to accept builtin integers. The archive retains the original submitted artifact as the reference result.
