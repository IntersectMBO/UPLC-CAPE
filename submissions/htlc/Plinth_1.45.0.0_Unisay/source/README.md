# HTLC Plinth Source

- **Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Paths**:
  - `lib/HTLC.hs` — spending validator
  - `lib/HTLC/Fixture.hs` — datum/redeemer types and test constants
  - `plinth-submissions-app/Main.hs` — compile splice that produces this submission's UPLC

## Reproducibility

```sh
cabal run plinth-submissions
# writes submissions/htlc/Plinth_1.45.0.0_Unisay/htlc.uplc
```

Requires the default `cabal.project` (plutus-tx-plugin 1.45 stack).
