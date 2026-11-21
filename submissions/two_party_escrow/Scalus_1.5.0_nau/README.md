# Benchmark Implementation Notes

**Scenario**: `two_party_escrow`

**Submission ID**: `Scalus_1.5.0_nau` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Scalus 1.5.0
- **Implementation Approach**: Scala 3 Validator with mint and spend: a thread token marks the escrow UTxO from Deposit until Accept or Refund burns it.
- **Compilation Flags**: `Options.releaseUntagged`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

### Source Code

- **Source Available**: true

#### For External Repository (recommended approach):

- **Direct Link**: [twopartyescrow](https://github.com/scalus3/scalus/tree/7f0d171797f0d705fef5bef38caa968116b45e65/scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/)
- **Repository**: https://github.com/scalus3/scalus
- **Tag/Commit**: `7f0d171797f0d705fef5bef38caa968116b45e65`
- **Source Path**: `scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/`

### Build Instructions

See [source/README.md](source/README.md): a scala-cli build that compiles the validator at this commit with the released Scalus 1.5.0 compiler from Maven Central.

### Compilation Configuration

- **Optimization Level**: release
- **Compilation Flags**: `Options.releaseUntagged`
- **Notable Configuration**: Targets Plutus V3, protocol version 11 (vanRossem), plutus-core 1.1.0. No traces.

## Notes

Compiled from Scala 3 with the Scalus compiler plugin, Options.releaseUntagged (PV11/vanRossem target: flexible case, batch-6 builtins), no traces. Scenario revision 2: one script with a minting purpose (Deposit mints the thread token, Burn destroys it) and a spending purpose (Accept, Refund). It reads its own hash from the context: the policy id when minting, the script hash of the spent input when spending.
