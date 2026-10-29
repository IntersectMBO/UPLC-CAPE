# Benchmark Implementation Notes

**Scenario**: `factorial`

**Submission ID**: `Scalus_1.4.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.4.0
- **Implementation Approach**: Hand-written UPLC term: memoized 0!..12! base case (PV11 case-on-builtin-integer) plus self-application recursion for x >= 13, so it's correct for any integer input, not just the benchmark's fixture range. Optimized by the Scalus UPLC pipeline.
- **Compilation Flags**: `Options.release`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

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

Hand-crafted UPLC: memoized base case + computed fallback, correct for every integer input. x < 0 -> 1; 0 <= x <= 12 -> O(1) lookup via PV11 case-on-builtin-integer (0!..12! baked in as direct case branches, no bytestring decode step); x >= 13 -> genuine self-application recursion (x * factorial(x - 1)) bottoming out on the table. A PV9-compatible sliceByteString/byteStringToInteger encoding of the table was also measured and lost decisively (109 vs 91 bytes; 18.4M vs 4.5M summed steps across the 10 fixture cases) -- see FactorialOpen.scala.
