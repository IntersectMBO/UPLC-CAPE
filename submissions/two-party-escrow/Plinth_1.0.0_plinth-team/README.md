# Benchmark Implementation Notes

**Scenario**: `two-party-escrow`

**Submission ID**: `Plinth_1.0.0_plinth-team` (Format: `Language_Version_GitHubHandle`)

## Implementation Details

- **Compiler**: Plinth (PlutusTx) 1.0.0 with Plutus 1.52.0.0
- **Implementation Approach**: Validator using redeemer-based state machine pattern
- **Compilation Flags**: `conservative-optimisation`, `remove-trace`, `target-version=1.1.0`

## Architecture

The Two-Party Escrow validator implements a simplified state machine with three operations:

- **Deposit** (redeemer = 0): Buyer deposits 75 ADA into escrow
- **Accept** (redeemer = 1): Seller accepts payment and completes transaction
- **Refund** (redeemer = 2): Buyer reclaims funds after deadline

The validator has a signature of `BuiltinData -> BuiltinUnit` and uses `unsafeDataAsI` to parse the redeemer integer. Fixed parameters (buyer/seller addresses, price, deadline) are baked into the compiled UPLC program.

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Reproducibility

- **Source Available**: `true`
- **Source Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Compilation Config**: Standard PlutusTx with conservative optimizations, targeting Plutus Core 1.1.0

## Notes

This is a reference implementation demonstrating the Two-Party Escrow benchmark specification. The validator provides basic structure for escrow operations but does not include full on-chain validation logic (signature verification, UTxO handling, etc.) that would be required in a production escrow contract. The implementation focuses on computational patterns relevant for performance benchmarking.

- Source code available in the `lib/TwoPartyEscrow.hs` module
- Compiled with PlutusTx plugin targeting Plutus Core 1.1.0
- Uses driver-based measurement architecture for accurate performance isolation
