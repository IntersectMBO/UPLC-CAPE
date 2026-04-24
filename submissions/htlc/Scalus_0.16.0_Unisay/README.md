# Benchmark Implementation Notes

**Scenario**: `htlc`

**Submission ID**: `Scalus_0.16.0_Unisay`

## Implementation Details

- **Compiler**: `Scalus 0.16.0`
- **Implementation Approach**: `idiomatic @Compile spending validator, Data -> Unit, derived FromData/ToData`
- **Compilation Flags**: `default`

## Performance Results

- See [metrics.json](metrics.json) for detailed performance measurements

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions

## Notes

Scalus spending validator matching the production-safe validity-range convention introduced in #170: claim reads the upper bound of `txInfoValidRange` (finite, strictly `< timeout`); refund reads the lower bound (finite, strictly `> timeout`). Datum and redeemer shapes are identical to the Plinth reference (`HTLCDatum = 0(payer, recipient, secretHash, timeout)`; `Claim(preimage) = 0(preimage)`, `Refund = 1()`). The source is maintained in a separate repository to avoid duplication.
