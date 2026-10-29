# Benchmark Implementation Notes

**Scenario**: `factorial_naive_recursion`

**Submission ID**: `Scalus_1.4.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.4.0
- **Implementation Approach**: Direct @Compile of the prescribed naive recursion.
- **Compilation Flags**: `Options.release`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Algorithm Compliance

`FactorialBase.scala` translates the prescribed algorithm one to one: the same base case, and the same
recursive calls with the same arguments. The Scalus compiler adds only general-purpose
optimizations, such as the recursion encoding and inlining.

```scala
package scalus.examples.cape.factorial

import scalus.compiler.Compile

import scalus.*

/** CAPE factorial_naive_recursion scenario.
  *
  * Prescribed naive recursive algorithm for compiler comparison.
  */
@Compile
object FactorialBase {
    def factorial(n: BigInt): BigInt =
        if n <= 0 then 1 else n * factorial(n - 1)
}
```

## Reproducibility

### Source Code

- **Source Available**: true

#### For External Repository (recommended approach):

- **Direct Link**: [factorial](https://github.com/scalus3/scalus/tree/f1fa4a721ae7a97d78b57ce651550be0846a792e/scalus-examples/jvm/src/main/scala/scalus/examples/cape/factorial/)
- **Repository**: https://github.com/scalus3/scalus
- **Tag/Commit**: `f1fa4a721ae7a97d78b57ce651550be0846a792e`
- **Source Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/factorial/`

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

Compiled from Scala 3 with the Scalus compiler plugin, Options.release (PV11/vanRossem target: flexible case, batch-6 builtins), no traces.
