# linear_vesting Plinth 1.45.0.0 (monadic, promoted to default) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `162d30fb5919d82b3b27d2b5b0b50ffc0b689b25`

**Path**: `lib/LinearVesting/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Linear vesting validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named` (`V.do`/`N.do` via `QualifiedDo`; `IxDecoder`/`FieldAt` Peano-indexed cursor; `Plinth.Encoded` compare-without-decode). Re-optimised for CAPE metrics schema 2.0.0 (happy-path aggregates only): total_fee 250376 to 85483 lovelace (-65.9%), promoted to the default for this line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 162d30fb5919d82b3b27d2b5b0b50ffc0b689b25
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout. If it is unset, the generator writes to `../UPLC-CAPE`.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/linear_vesting/Plinth_1.45.0.0_Unisay/linear_vesting.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `linear_vesting.uplc` in this submission.
