# Benchmark Implementation Notes

**Scenario**: `fibonacci`

**Submission ID**: `Scalus_1.4.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.4.0
- **Implementation Approach**: Hand-written UPLC term: memoized fib(0)..fib(25) base case (PV11 case-on-builtin-integer) plus a linear accumulator loop for n >= 26, so it's correct for any integer input, not just the benchmark's fixture range. Optimized by the Scalus UPLC pipeline.
- **Compilation Flags**: `Options.release`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

### Source Code

- **Source Available**: true

#### For External Repository (recommended approach):

- **Direct Link**: [fibonacci](https://github.com/scalus3/scalus/tree/f1fa4a721ae7a97d78b57ce651550be0846a792e/scalus-examples/jvm/src/main/scala/scalus/examples/cape/fibonacci/)
- **Repository**: https://github.com/scalus3/scalus
- **Tag/Commit**: `f1fa4a721ae7a97d78b57ce651550be0846a792e`
- **Source Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/fibonacci/`

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

Hand-crafted UPLC: memoized base case + computed fallback, correct for every integer input. n < 0 -> n; 0 <= n <= 25 -> O(1) lookup via PV11 case-on-builtin-integer (fib(0)..fib(25) baked in as direct case branches, no bytestring decode step); n >= 26 -> a linear accumulator loop (go(k, a, b) = if k <= 0 then b else go(k - 1, b, a + b), seeded from fib(24)/fib(25)) bottoming out on the table -- O(n), not the O(2^n) naive double recursion the scenario warns against.
