# Script Contexts for Scripts with Several Purposes

- Status: accepted
- Deciders: Unisay
- Date: 2026-11-13
- Tags: tests, schema, scenarios

Technical Story: [#272](https://github.com/IntersectMBO/UPLC-CAPE/issues/272)

## Context and Problem Statement

A `cape-tests.json` suite builds a `ScriptContext` from a baseline and a list of patches. The only baseline is `spending`, and the builder fills in what a test does not say with placeholders: the script's own input sits at a fixed 58-byte "script hash", transaction ids are the 64 characters of their hex text, the fee is 0, and `txInfoMint` and `txInfoRedeemers` are empty.

Revision 2 of `two_party_escrow` is one script with a minting and a spending purpose. The policy id of its token is the hash of the script, so the script has to read its own hash from the context. The format could express none of this: no purpose except spending, no mint field, no way to place an input at a chosen script hash.

**Question**: how does a suite describe a script with several purposes, in contexts that the ledger could have built?

## Decision Drivers

- The `htlc` and `linear_vesting` suites must build the same contexts as before, byte for byte. Their metrics must not move.
- An accept transaction runs the escrow script twice, for the input and for the burn. Both runs must see the same transaction.
- A validator that decodes the whole context pays for every field of it. A context with an empty redeemers map measures less than the ledger charges.
- A rule that the scenario leaves open, for example "the escrow output may carry a reference script", needs a test. An untested permission lets a stricter and cheaper implementation pass.
- The format stays declarative. A test reads as a transaction, not as raw `Data`.

## Considered Options

1. A second baseline, `minting`
2. Patches on the one existing baseline
3. Contexts as raw `builtin_data`
4. New defaults for the existing baseline

## Decision Outcome

Chosen option: "Patches on the one existing baseline". A context starts as a spending context, as before, and ten additions change what the placeholders used to fix. Every addition is opt-in: a suite that does not use it builds the same bytes as before.

| Addition | Effect |
| --- | --- |
| Patch `set_script_info` | Switches the purpose to `minting` (with a `policy_id`) or `rewarding` (with a `credential`). The transaction stays as it is. |
| Patch `set_mint` | Sets `txInfoMint` from `assets`, the format a value uses. Quantities are signed. |
| Patch `set_fee` | Sets `txInfoFee`. |
| Patch `set_tx_id` | Sets `txInfoId`. |
| Patch `set_redeemers` | Sets `txInfoRedeemers` from a list of purposes (`spending`, `minting`, `rewarding`) and redeemers, in the order given. |
| `address` on `add_input_utxo` | Places an input at any address. |
| `reference_script` on `add_input_utxo` and `add_output_utxo` | Gives the UTXO a reference script hash. |
| `staking` in an address | Adds a staking credential. |
| `@reference` in `script_hash` | Resolves to the bytes of a `B #hex` value, as `pubkey_hash` already did. |
| `#` before the transaction id in `utxo_ref` | Reads the id as hex: 64 digits give 32 bytes. |

One transaction under two purposes is one shared context plus one patch:

```json
{
  "baseline": "@successful_accept",
  "patches": [
    {
      "op": "set_script_info",
      "script_info": { "type": "minting", "policy_id": "@escrow_script_hash" }
    }
  ]
}
```

Details that a suite author needs:

- **Old literals keep their reading.** A `script_hash` or `pubkey_hash` literal is still taken as its raw ASCII bytes, and a `utxo_ref` without `#` still uses the characters of the id. Only the new forms give real bytes. A reference that does not point to a bytestring fails; it does not fall back to ASCII.
- **`set_mint` builds what the ledger builds.** It goes through `PlutusCore.Value`, the path `buildValue` takes: policies and token names in ascending byte order, duplicates summed, zero quantities dropped. The result has no ADA entry, and ADA in `assets` is an error.
- **`set_redeemers` and `set_redeemer` are independent.** The first is the map that every script of the transaction sees. The second is the redeemer of the run under test. A suite keeps them consistent.
- **Inputs, outputs and signatories are still prepended.** A suite lists them in reverse to get the ledger's order ([#271](https://github.com/IntersectMBO/UPLC-CAPE/issues/271)).

`cape submission verify` now validates the scenario's suite against `schemas/cape-tests.schema.json`.

### Positive Consequences

- A suite can describe a minting policy, a script with two purposes, and a transaction with realistic sizes and a complete redeemers map.
- Existing suites are untouched: the `htlc` and `linear_vesting` contexts are identical before and after, under both the production and the preview evaluator.
- A scenario can test what it permits: staking credentials, reference scripts, mints of other policies.

### Negative Consequences

- A hash has two readings, bytes behind a reference and ASCII in a literal. This stays until the placeholders themselves change ([#273](https://github.com/IntersectMBO/UPLC-CAPE/issues/273)).
- The builder does not check that a context is one the ledger could build: sorted inputs, conserved value, a redeemer for each script run. A scenario that depends on it checks its own fixtures, as `test/Cape/TwoPartyEscrowFixturesSpec.hs` does.
- Withdrawals, certificates, votes, proposals, reference inputs and datum hashes still cannot be expressed.

## Pros and Cons of the Options

### A second baseline, `minting`

- Good, because a minting test names its purpose in the first line.
- Bad, because the burn run and the spending run of one transaction would start from two baselines. The suite would state the transaction twice, and nothing would keep the two copies equal.

### Patches on the one existing baseline

- Good, because the purpose is one more property of a context, changed like the others.
- Good, because one transaction serves every purpose it triggers.
- Bad, because a minting context starts its life as a spending context, which reads oddly.

### Contexts as raw `builtin_data`

- Good, because it needs no change to the format.
- Bad, because a 3 KB `Data` term per test cannot be reviewed, and a suite could no longer share and patch contexts.

### New defaults for the existing baseline

Give the placeholders real sizes for every scenario.

- Good, because one reading of each field remains.
- Bad, because it changes the contexts of `htlc` and `linear_vesting` and so their metrics. That is the subject of [#273](https://github.com/IntersectMBO/UPLC-CAPE/issues/273), not of this decision.

## Links

- Refines [Structural address format for ScriptContext test cases](20250826-structural-address-format-for-scriptcontext-test-cases.md)
- [#271](https://github.com/IntersectMBO/UPLC-CAPE/issues/271): the builder should produce the ledger's order
- [#273](https://github.com/IntersectMBO/UPLC-CAPE/issues/273): placeholder values that the ledger never produces
