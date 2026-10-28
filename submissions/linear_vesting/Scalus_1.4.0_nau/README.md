# Benchmark Implementation Notes

**Scenario**: `linear_vesting`

**Submission ID**: `Scalus_1.4.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.4.0
- **Implementation Approach**: Scala 3 validator implementing the ceiling-division vesting schedule.
- **Compilation Flags**: `Options.release`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

### Source Code

- **Source Available**: true

#### For External Repository (recommended approach):

- **Direct Link**: [linearvesting](https://github.com/scalus3/scalus/tree/f1fa4a721ae7a97d78b57ce651550be0846a792e/scalus-examples/jvm/src/main/scala/scalus/examples/cape/linearvesting/)
- **Repository**: https://github.com/scalus3/scalus
- **Tag/Commit**: `f1fa4a721ae7a97d78b57ce651550be0846a792e`
- **Source Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/linearvesting/`

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

Compiled from Scala 3 with the Scalus compiler plugin, Options.release (PV11/vanRossem target: flexible case, batch-6 builtins), no traces. Value.quantityOf lowers to the CIP-0153 lookupCoin/unValueData builtins (Options.valueBuiltins, the PV11 default).
