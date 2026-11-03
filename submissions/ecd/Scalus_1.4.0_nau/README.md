# Benchmark Implementation Notes

**Scenario**: `ecd`

**Submission ID**: `Scalus_1.4.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.4.0
- **Implementation Approach**: Direct @Compile of the prescribed naive recursive GCD.
- **Compilation Flags**: `Options.release`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Algorithm Compliance

`EcdBase.scala` translates the prescribed algorithm one to one: the same base case, and the same
recursive calls with the same arguments. The Scalus compiler adds only general-purpose
optimizations, such as the recursion encoding and inlining.

```scala
package scalus.examples.cape.ecd

import scalus.compiler.Compile
import scalus.*
import scalus.uplc.builtin.Builtins.modInteger

/** CAPE `ecd` scenario: prescribed naive recursive Euclidean algorithm.
  *
  * Direct translation of the spec (fixed mode):
  * {{{ecd a b | b == 0 = abs a | otherwise = ecd b (a `mod` b)}}}
  *
  * Haskell's `mod` takes the sign of the divisor, like the `modInteger` builtin. Scala's `%` is
  * `rem`, which takes the sign of the dividend and changes the recursive arguments for negative
  * inputs: `ecd(-12, 8)` must recurse into `ecd(8, 4)`, not `ecd(8, -4)`.
  */
@Compile
object EcdBase {
    def ecd(a: BigInt, b: BigInt): BigInt =
        if b == BigInt(0) then if a < 0 then -a else a
        else ecd(b, modInteger(a, b))
}
```

## Reproducibility

### Source Code

- **Source Available**: true

#### For External Repository (recommended approach):

- **Direct Link**: [ecd](https://github.com/scalus3/scalus/tree/f1fa4a721ae7a97d78b57ce651550be0846a792e/scalus-examples/jvm/src/main/scala/scalus/examples/cape/ecd/)
- **Repository**: https://github.com/scalus3/scalus
- **Tag/Commit**: `f1fa4a721ae7a97d78b57ce651550be0846a792e`
- **Source Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/ecd/`

### Build Instructions

```sh
git clone https://github.com/scalus3/scalus
cd scalus
git checkout f1fa4a721ae7a97d78b57ce651550be0846a792e
nix develop   # provides JDK 21 and sbt
sbt "scalusExamplesJVM/runMain scalus.examples.cape.GenerateSubmissions <path-to-UPLC-CAPE> 1.4.0"
```

This writes `submissions/<scenario>/Scalus_1.4.0_nau/` for all 8 scenarios: the `.uplc` file,
`metadata.json` and this README. Then run `treefmt` in the UPLC-CAPE checkout to apply the
`pretty-uplc` layout, which changes only whitespace. `scripts/cape-submit.sh` in the
Scalus repository runs all of this, then `cape submission verify` and `measure`.

### Compilation Configuration

- **Optimization Level**: release
- **Compilation Flags**: `Options.release`
- **Notable Configuration**: Targets Plutus V3, protocol version 11 (vanRossem), plutus-core 1.1.0. No traces.

## Notes

Compiled from Scala 3 with the Scalus compiler plugin, Options.release (PV11/vanRossem target: flexible case, batch-6 builtins), no traces. Direct translation of the prescribed Euclidean algorithm; the only compiler-automatic transforms are recursion encoding and inlining.
