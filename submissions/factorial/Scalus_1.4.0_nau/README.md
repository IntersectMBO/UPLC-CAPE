# Benchmark Implementation Notes

**Scenario**: `factorial`

**Submission ID**: `Scalus_1.4.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.4.0
- **Implementation Approach**: Scala 3 @Compile function: the factorial recursion unrolled four times, computing every result at run time, correct for any integer input.
- **Compilation Flags**: `Options.release`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

### Source Code

- **Source Available**: true

#### For External Repository (recommended approach):

- **Direct Link**: [factorial](https://github.com/scalus3/scalus/tree/9b26ab007d6b8790eb55da55cef3dd788c350a93/scalus-examples/jvm/src/main/scala/scalus/examples/cape/factorial/)
- **Repository**: https://github.com/scalus3/scalus
- **Tag/Commit**: `9b26ab007d6b8790eb55da55cef3dd788c350a93`
- **Source Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/factorial/`

### Build Instructions

```sh
git clone https://github.com/scalus3/scalus
cd scalus
git checkout 9b26ab007d6b8790eb55da55cef3dd788c350a93
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

Compiled from Scala 3 (@Compile, Options.releaseUntagged). Computes every result at run time, with the recursion unrolled four times: factorial(n) = n(n-1)(n-2)(n-3) * factorial(n - 4) for n >= 4, n(n-1) for n = 2 and 3, and 1 for every n <= 1, negatives included. No values are precomputed. factorial(12) takes three recursive calls instead of eleven.
