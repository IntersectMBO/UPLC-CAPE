# Scalus Two-Party Escrow Implementation

**Source Code**: [TwoPartyEscrow.scala](https://github.com/Unisay/scalus-cape-submissions/blob/39b36f12b1803f2f88529c20e00b573b3c21147b/src/two_party_escrow/TwoPartyEscrow.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `39b36f12b1803f2f88529c20e00b573b3c21147b`

**Path**: `src/two_party_escrow/TwoPartyEscrow.scala`

This submission uses Scalus compiler version 0.18.2. Revision 2 of the scenario (thread token): one Plutus V3 script (`@Compile object TwoPartyEscrowValidator`, `Data -> Unit`) that runs as a minting policy for the token and as a spending validator for the funds, and reads its own hash from the script context. Minting with redeemer 0 is the deposit and minting with redeemer 1 or 2 is the burn; spending with redeemer 1 is Accept and with redeemer 2 is Refund. The buyer and seller keys, the 75 ADA price, the 30-minute (1800000 ms) delay and the token name are compiled in; the datum is `Constr 0 [depositTime]`, where `depositTime` is the finite upper bound of the deposit validity range minus one millisecond. The validator enforces one escrow input per spend and sums payments by payment credential. Targets the van Rossem protocol version (Cardano protocol version 11), live on mainnet since 2026-07-18. The validator text is the same as in `Scalus_1.3.0_Unisay`, apart from imports. Compiled with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)` (Scala 3.3.7), which enables `case-on-builtins` and batch-6 builtins (for example `dropList`). The artifact reads token quantities from the `Data` maps and uses no CIP-153 Value builtin.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 39b36f12b1803f2f88529c20e00b573b3c21147b
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain two_party_escrow.compileTwoPartyEscrow'
   ```

   The `@main` writes both the changPV artifact (`two_party_escrow`) and the `-preview.uplc` into `src/two_party_escrow/`; this submission pins the `-preview.uplc` build.
