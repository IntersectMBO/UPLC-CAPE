# linear_vesting Plinth 1.45.0.0 (monadic, promoted to default) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.45`

**Commit**: `b10eb9c4be0745a199c1febe963e00631ca0c3d7`

**Path**: `lib/LinearVesting/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Linear vesting validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named` (`V.do`/`N.do` via `QualifiedDo`; `IxDecoder`/`FieldAt` Peano-indexed cursor; `Plinth.Encoded` compare-without-decode). Re-optimised for CAPE metrics schema 2.0.0 (happy-path aggregates only): total_fee 250376 to 85483 lovelace (-65.9%), promoted to the default for this line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout b10eb9c4be0745a199c1febe963e00631ca0c3d7
```

`CAPE_REPO` must point at the sibling UPLC-CAPE checkout (build aborts if unset); set it in `.envrc.local` (gitignored). Then:

```bash
nix develop
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/linear_vesting/Plinth_1.45.0.0_Unisay_monadic/linear_vesting.uplc`; in this submission that artifact is committed as the default `Plinth_1.45.0.0_Unisay` (the previous plain implementation is retained as `Plinth_1.45.0.0_Unisay_plain`).
