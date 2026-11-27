# linear_vesting Plinth 1.64.0.0 (monadic, promoted to default) source

**Repository**: <https://github.com/Unisay/plinth-cape-submissions>

**Branch**: `plinth-1.64`

**Commit**: `1999e3dc700416dce562dd2b841700ccdaa4424c`

**Path**: `lib/LinearVesting/Monadic.hs` (+ `lib/Plinth/Validator.hs`, `lib/Plinth/Decoder/Named.hs`, `lib/Plinth/Decoder/Named/ScriptContext.hs`, `lib/Plinth/Encoded.hs`)

Linear vesting validator written in `do`-notation on `Plinth.Validator`, a zero-cost early-termination monad, together with the zero-cost typed decoding DSL `Plinth.Decoder.Named` (`V.do`/`N.do` via `QualifiedDo`; `IxDecoder`/`FieldAt` Peano-indexed cursor; `Plinth.Encoded` compare-without-decode). Re-optimised for CAPE metrics schema 2.0.0 (happy-path aggregates only): total_fee 192239 to 65598 lovelace (-65.9%), promoted to the default for this line.

## Reproducing the compilation

```bash
git clone https://github.com/Unisay/plinth-cape-submissions
cd plinth-cape-submissions
git checkout 1999e3dc700416dce562dd2b841700ccdaa4424c
```

`CAPE_REPO` points the generator at the UPLC-CAPE checkout; the generator stops if it is unset. You can also set it in `.envrc.local`, which the branch's `.envrc` loads.

```bash
nix develop
export CAPE_REPO="$HOME/src/UPLC-CAPE"
cabal run plinth-submissions
```

The generator writes `$CAPE_REPO/submissions/linear_vesting/Plinth_1.64.0.0_Unisay/linear_vesting.uplc`. After formatting with CAPE's `pretty-uplc`, it equals the `linear_vesting.uplc` in this submission.
