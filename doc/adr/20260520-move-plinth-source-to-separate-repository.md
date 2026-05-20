# Move Plinth source tree to a separate repository

- Status: accepted
- Deciders: Yuriy Lazaryev (Unisay)
- Date: 2026-05-20
- Tags: plinth, repository-structure, submissions, evaluator

Technical Story: <https://github.com/IntersectMBO/UPLC-CAPE/issues/193>

## Context and Problem Statement

UPLC-CAPE played two roles for Plinth that no other compiler in the benchmark
suite (Scalus, Aiken, Plutarch, …) imposed: it stored compiled `.uplc`
artefacts and measurements *and* it hosted the Haskell source tree,
plutus-tx-plugin pragmas, and `cabal.project` machinery that produced those
artefacts. Two unrelated axes were conflated in the build matrix here:

- target evaluator surface (mainnet `plutus-core` vs an upcoming-HF
  `plutus-core`);
- compiler version used to emit UPLC (Plinth 1.45 vs Plinth 1.61, with 1.64
  pending).

`cabal.project` and `cabal.project.preview` mixed both axes, and every new
Plinth release multiplied the build surface here even though the only durable
output is the `.uplc` plus its measurements. Other compiler submissions
(`Scalus_*_Unisay`, `Aiken_*_*`) already follow a thinner pattern: each
submission's `source/README.md` points at an external repository at a
specific commit, and the only files committed in UPLC-CAPE are the artefact,
metrics, and a short README.

## Decision Drivers

- Lower the build / onboarding cost for non-Plinth contributors to UPLC-CAPE.
- Allow the Plinth source to follow its own release cadence (1.64 is current;
  more bumps expected as `plutus-tx-plugin` releases continue).
- Align Plinth with the Scalus / Aiken layout so the framework treats every
  compiler symmetrically.
- Keep the .uplc artefacts exactly reproducible from a single named commit
  per archive line.

## Considered Options

- Keep Plinth source in-tree and continue maintaining one
  `cabal.project.<line>` file per Plinth release.
- Move Plinth source to a separate repository with archive branches frozen at
  the commit that produced each `Plinth_<ver>_Unisay/*.uplc` currently in
  UPLC-CAPE, and a `main` branch that collapses the preview axis into a
  cabal flag.

## Decision Outcome

Chosen option: **separate repository with archive branches**, hosted at
<https://github.com/Unisay/plinth-cape-submissions>. UPLC-CAPE keeps the
evaluator (`measure`, `measure-preview`, `pretty-uplc`), the measurement
library, the test suite, the `cape` CLI, and the published `.uplc` /
`metrics.json` / `metadata.json` / `source/README.md` files per submission.

The Plinth submission repository has three branches:

- `main` — current production line (Plinth 1.64.0.0). The preview axis
  (`BuiltinCasing`) is now a cabal flag (`--flags=preview`) that only changes
  GHC options and the destination subdirectory; there is no parallel
  `lib/Preview/` tree on `main`.
- `plinth-1.45` — frozen snapshot that reproduces byte-identical UPLC for
  every `submissions/<sc>/Plinth_1.45.0.0_Unisay/<sc>.uplc` in this repo.
- `plinth-1.61` — same shape for `Plinth_1.61.0.0_Unisay/*.uplc`.

Each Plinth submission's `source/README.md` here points at the relevant
branch + commit SHA + module path, matching the existing Scalus / Aiken
pattern.

UPLC-CAPE no longer depends on `plutus-tx-plugin`. `flag preview`,
`executable measure-preview`, and `cabal.project.preview` are intentionally
kept: `measure-preview` is the evaluator used to score
`Plinth_1.61.0.0_Unisay/*.uplc` (and any other ≥1.46 UPLC) against a newer
`plutus-core` baseline. That dual-resolver setup is orthogonal to where the
Plinth source lives.

### Positive Consequences

- Adding or measuring a non-Plinth submission no longer pulls in
  `plutus-tx-plugin`, `plutus-metatheory`, or the rest of the Plinth build
  surface.
- The Plinth repo tracks the latest released Plinth on its own cadence; this
  repo does not need to be touched for every Plinth bump.
- The repository layout is symmetric across all compilers; there is no
  privileged in-tree compiler.
- Reproducibility of historical Plinth measurements is anchored to immutable
  branch SHAs rather than to the current state of `lib/`.

### Negative Consequences

- Editing a Plinth validator now requires two checkouts side-by-side
  (`UPLC-CAPE/` and `plinth-cape-submissions/`) and an explicit `CAPE_REPO`
  env var when running the generator.
- Cross-repo PRs (source change in the Plinth repo, `.uplc` + `metrics.json`
  update here) are now a two-step workflow. Re-measuring existing `.uplc`
  remains a UPLC-CAPE-only operation.

## Pros and Cons of the Options

### Keep Plinth source in-tree

- Good, because a single checkout produces and measures Plinth submissions.
- Bad, because every Plinth release multiplies the in-tree build matrix.
- Bad, because non-Plinth contributors still pay the
  `plutus-tx-plugin` / `plutus-metatheory` build cost.
- Bad, because the layout is asymmetric: Plinth is privileged, every other
  compiler (Scalus, Aiken, Plutarch) is external.

### Separate repository with archive branches (chosen)

- Good, because it matches the existing Scalus / Aiken pattern.
- Good, because immutable archive branches encode "this is exactly the
  source state that produced the committed UPLC".
- Good, because the cabal-flag preview collapse on `main` removes the
  parallel `lib/Preview/` source tree (was a mechanical re-compile only).
- Good, because the .uplc → repository contract is now identical across
  compilers, simplifying the framework's domain model.
- Bad, because the developer workflow needs two checkouts side by side.

## Links

- Implementation issue: <https://github.com/IntersectMBO/UPLC-CAPE/issues/193>
- Plinth submission repository:
  <https://github.com/Unisay/plinth-cape-submissions>
- Scalus submission repository (existing precedent):
  <https://github.com/Unisay/scalus-cape-submissions>
