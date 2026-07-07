# Scalus Two-Party Escrow Implementation

**Source Code**: [TwoPartyEscrow.scala](https://github.com/Unisay/scalus-cape-submissions/blob/6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5/src/two_party_escrow/TwoPartyEscrow.scala)

**Repository**: <https://github.com/Unisay/scalus-cape-submissions>

**Commit**: `6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5`

**Path**: `src/two_party_escrow/TwoPartyEscrow.scala`

This submission uses Scalus compiler version 0.18.2. Two-party escrow spending validator (`@Compile object TwoPartyEscrowValidator`, `Data -> Unit`) implementing a buyer/seller `Deposited -> Accepted | Refunded` state machine with parameters baked in (buyer/seller keys, 75 ADA price, 1800s deadline). Redeemer is a raw integer (0=Deposit, 1=Accept, 2=Refund); datum is `Constr 0 [state, depositTime]`. Deposit records `depositTime` as the finite upper bound of the validity range (an infinite upper bound is rejected). Preview submission targeting the van Rossem hard fork (Cardano protocol version 11). Same source as the corresponding current-track Scalus_0.18.2_Unisay submission, recompiled with `Options.release.copy(targetProtocolVersion = MajorProtocolVersion.vanRossemPV)`, which enables `case-on-builtins` and batch-6 builtins (e.g. `dropList`). Measured on `PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)`. Invalid on mainnet until the hard fork activates; routed to the preview report via `min_plutus_version = 1.60.0.0`.

## Reproducing the Compilation

1. Clone the repository:

   ```bash
   git clone https://github.com/Unisay/scalus-cape-submissions
   cd scalus-cape-submissions
   ```

2. Check out the specific commit:

   ```bash
   git checkout 6aa01a8ab6dbf0501efdea5bca4e3b832a1307b5
   ```

3. Build the artifact (nix shell `build-scalus`, or per scenario):

   ```bash
   sbt 'runMain two_party_escrow.compileTwoPartyEscrow'
   ```

   The `@main` writes both the changPV artifact (`two_party_escrow`) and the `-preview.uplc` into `src/two_party_escrow/`; this submission pins the `-preview.uplc` build.
