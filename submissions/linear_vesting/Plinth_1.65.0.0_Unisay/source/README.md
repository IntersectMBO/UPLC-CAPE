# linear_vesting Plinth 1.65.0.0 source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `main`

**Commit**: `f9ef6bdf4ed08ec16039600903d8af7e6c22046b`

**Path**: `lib/LinearVesting.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Linear vesting validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named` (`V.do`/`N.do` via `QualifiedDo`; `IxDecoder`/`FieldAt` Peano-indexed cursor; `Plinth.Encoded` compare-without-decode). Re-optimised for CAPE metrics schema 2.0.0 (happy-path aggregates only): This round's structural change decodes the `VestingDatum` on the partial-unlock path in a single 7-field region. total_fee 173470 to 59048 lovelace (-66.0%), promoted to the default for this line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout f9ef6bdf4ed08ec16039600903d8af7e6c22046b
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/linear_vesting/Plinth_1.65.0.0_Unisay/linear_vesting.uplc` (monadic is the default on `main`; the previous plain implementation is retained as `Plinth_1.65.0.0_Unisay_plain`).
