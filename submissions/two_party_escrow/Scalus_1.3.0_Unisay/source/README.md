# Scalus Two-Party Escrow Implementation

**Source Code**: [TwoPartyEscrow.scala](https://github.com/Unisay/scalus-cape-submissions/blob/75678ebd8a1d627102095c4df1b72b774d0ccee8/src/two_party_escrow/TwoPartyEscrow.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Branch**: `main`

**Commit**: `75678ebd8a1d627102095c4df1b72b774d0ccee8`

**Path**: `src/two_party_escrow/TwoPartyEscrow.scala`

This submission uses Scalus compiler version 1.3.0. Two-party escrow spending validator (`@Compile object TwoPartyEscrowValidator`, `Data -> Unit`) implementing a buyer/seller `Deposited -> Accepted | Refunded` state machine with parameters baked in (buyer/seller keys, 75 ADA price, 30-minute (1800000 ms) deadline). Redeemer is a raw integer (0=Deposit, 1=Accept, 2=Refund); datum is `Constr 0 [state, depositTime]`. Deposit records `depositTime` as the finite upper bound of the validity range (an infinite upper bound is rejected). Targets the van Rossem protocol version (Cardano protocol version 11), live on mainnet since 2026-07-18. Compiled once with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)` (Scala 3.3.8, `scalus-plugin` 1.3.0); there is no separate preview build any more. Compiler changes since 0.18.2 include recursion through self-application instead of the Z combinator, the static-argument transformation, the unified UPLC pipeline, and fee-aware CSE and inlining. The validator reads only lovelace, which Scalus does not lower to the CIP-153 Value builtins.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 75678ebd8a1d627102095c4df1b72b774d0ccee8
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain two_party_escrow.compileTwoPartyEscrow'
   ```

   The `@main` compiles once and writes `src/two_party_escrow/two_party_escrow.uplc`, which matches the UPLC in this submission.
