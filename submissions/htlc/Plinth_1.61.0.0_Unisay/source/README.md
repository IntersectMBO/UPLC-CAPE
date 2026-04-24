# HTLC Plinth Source (Preview)

- **Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Paths**:
  - `lib/HTLC.hs` — spending validator (shared with the 1.45 submission)
  - `lib/HTLC/Fixture.hs` — datum/redeemer types and test constants
  - `lib/Preview/HTLC.hs` — recompilation module with `BuiltinCasing`
  - `plinth-submissions-app/Main.hs` — compile splice that produces this submission's UPLC

## Reproducibility

```sh
cabal run --project-file=cabal.project.preview -f preview plinth-submissions
# writes submissions/htlc/Plinth_1.61.0.0_Unisay/htlc.uplc
```

Requires the preview cabal project (plutus-tx-plugin 1.61 stack with `BuiltinCasing` support).
