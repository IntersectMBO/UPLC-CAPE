# Evolution Report — Per-Compiler Version-Evolution View

- Status: accepted
- Date: 2026-05-26
- Tags: reporting, html, ux, compilers

Technical Story: [#204](https://github.com/IntersectMBO/UPLC-CAPE/issues/204)

## Context and Problem Statement

The `preview/` report (`preview/index.html` and `preview/compilers/<compiler>_<author>.html`) was built around a binary comparison: one production version vs one preview version, with deltas per benchmark. The data shape `build_compiler_comparison_stats` produced and the HTML it fed baked two assumptions:

1. A `(compiler, author)` pair has exactly one preview version. The compiler page picked it via `head -1` over preview CSV rows; the title showed that single version.
2. The interesting comparison is prod vs preview, with deltas computed from the matching prod row.

Both broke as soon as a `(compiler, author)` pair gained more than one preview version. PR [#199](https://github.com/IntersectMBO/UPLC-CAPE/pull/199) added `Plinth_1.64.0.0_Unisay_builtincasing/` alongside `Plinth_1.61.0.0_Unisay/`; the deployed `pr-199/preview/compilers/Plinth_Unisay.html` ended up with the title "Preview 1.61.0.0" while the scenario table mixed 1.61 and 1.64 rows under the same benchmark names (duplicates). With Plinth 1.65.0.0 about to land and additional Scalus variants on the horizon, this layout will not scale.

**Question**: how do we present a compiler's submission history when multiple versions and variants coexist on either side of the mainnet/preview cut-line?

## Decision Drivers

- **No special-casing of any version**: production and preview are both points on a timeline; neither should be privileged in the data model or the UI.
- **Multiple versions per `(compiler, author)`** must render cleanly — current Plinth_Unisay already has 1.45 (current) + 1.61 (preview) + 1.64 (current + preview/builtincasing).
- **Per-variant integrity of deltas**: deltas only make sense within the same variant (a `builtincasing` variant is a different program from the `default` variant), so cross-variant deltas must not be computed implicitly.
- **Reader cognition**: a reviewer opening the page wants to answer "did the new release improve this scenario?" without first decoding which column is the baseline.
- **No infinite history**: scenarios that only appear once give no improvement signal and clutter the variant table.

## Considered Options

### 1. Patch the existing prod-vs-preview view (status-quo extension)

Keep the binary comparison and add a "pick which preview version to show" selector or repeat the table per preview version.

- Good, because it is a small diff and inbound `preview/` URLs keep working.
- Bad, because the data model still treats production as special, hiding the cross-version improvement signal that motivated the redesign.
- Bad, because it does not solve duplicate-row mixing if two preview versions both contribute to the same scenario.

### 2. Per-`(compiler, author)` evolution page, variant as a section

One page per `(compiler, author)`. The page has one section per variant. Each variant section renders a per-scenario table whose columns are the variant's versions in semver order. Cells show the absolute metric plus a delta against the previous column (default) or against the first column (toggle).

- Good, because the timeline naturally extends as new versions ship — production / preview is just a `min_plutus_version` attribute on each version, not a top-level partition.
- Good, because deltas remain semantically clean: every comparison is within the same variant.
- Good, because the visual structure mirrors how reviewers actually think about a compiler ("how has Plinth/default evolved?").
- Bad, because it forces a directory rename (`preview/` → `evolution/`), so deep links from external docs need updating.

### 3. Per-`(compiler, author, variant)` page, one variant per URL

Treat the variant as part of the routing key, producing pages like `evolution/compilers/Plinth_Unisay_default.html` and `evolution/compilers/Plinth_Unisay_builtincasing.html`.

- Good, because each page is single-purpose and the URL encodes the full context.
- Bad, because most variants have only one or two versions today, so most pages are sparse and the index grows linearly with variants.
- Bad, because the "compare default vs an experimental variant for the same compiler" workflow now requires opening two tabs.

## Decision Outcome

Chosen option: **Option 2 — per-`(compiler, author)` evolution page, variant as a section**, because it removes the prod-vs-preview asymmetry from the data model, keeps deltas within a single variant, and keeps the URL space compact (one page per author, however many variants they have).

### Scope of the change

- Replace `build_compiler_comparison_stats` in `scripts/cape-subcommands/submission/report.sh` with `build_evolution_stats`. The new helper aggregates the unfiltered submission CSV, groups rows by `(compiler, author, variant)`, sorts versions with `sort -V`, and computes both delta-vs-previous and delta-vs-first percentages for each of `cpu_units`, `memory_units`, `script_size_bytes`, `term_size`, `total_fee_lovelace`.
- Rename `report/preview/` → `report/evolution/`. The top-level `index.html` nav link, footer link, and `compilers/*.html` breadcrumbs all switch over.
- Drop `index-preview.html.tmpl`, `compiler-preview.html.tmpl`, and the unused `benchmark-preview.html.tmpl`. Add `index-evolution.html.tmpl` and `compiler-evolution.html.tmpl`.
- The new per-compiler template embeds both delta sets in its data; a small JS toggle re-renders the visible delta values when the reader flips between "vs previous" and "vs first". No round-trip to the server.

### Design rules applied

- **Baseline default**: deltas vs the previous version, with a UI toggle for vs first.
- **Scenario filter**: a scenario is rendered for a given variant only when it has at least two versions of data. Single-version rows have no improvement signal and are dropped (this also drops entire variants that have just one version across all scenarios).
- **Variant labeling**: each variant gets its own `<section>` titled with the variant name; `default` variants are labeled "default".
- **CSV target filter**: `cape submission aggregate --target=preview` keeps its role for callers that want only preview rows, but the evolution report itself uses unfiltered aggregation.

### Positive Consequences

- Plinth_Unisay's page now shows the natural 1.45 → 1.61 → 1.64 progression for the `default` variant; the `builtincasing` row (only 1.64 today) is dropped until a second `builtincasing` version exists, at which point it appears organically.
- New compiler releases (e.g. Plinth 1.65 with the polymorphic builtin lifting pass) require no template changes — they just become a new column in the existing variant table.
- The data model stops privileging mainnet. Promotion of a preview Plutus version to mainnet only flips the `min_plutus_version` interpretation, not the visual layout.

### Negative Consequences

- Existing inbound links to `pr-*/preview/...` URLs break. PR-preview deployments are short-lived, so the blast radius is limited, but any external bookmark to the main-branch `preview/` directory will 404.
- The "delta vs first" toggle is computed client-side, which adds a small JS payload (~1 KB) to a previously script-free page.
- Variants with a single version are invisible in the report. This is intentional but worth noting: contributors who add a brand-new variant will not see it on the evolution page until a second version of that variant lands.

## Refinement (2026-06-22)

Visual review of the deployed `pr-209/evolution/compilers/Plinth_Unisay.html` surfaced a real semantic problem with the originally-shipped data model: the timeline for `(Plinth, Unisay, default)` rendered `1.45.0.0 → 1.61.0.0 → 1.64.0.0 → 1.65.0.0` as a single chain, reading 1.61→1.64 as a "+28.6% CPU regression". 1.61 is a preview-only submission compiled against a different cost model with the `BuiltinCasing` flag — features mainnet cannot execute. Mixing it into a mainnet-baseline timeline is apples-to-grapes and obscures the actual mainnet evolution (1.45 → 1.64 → 1.65 shows the expected monotonic improvement).

Two refinements address this:

### Mainnet/preview split

The timeline is now mainnet-only. Submissions are partitioned by the existing `cape_is_preview_submission` semantics — a submission is **preview** when its `compilation_config.min_plutus_version` exceeds `CAPE_CURRENT_PLUTUS_VERSION` (defined in `scripts/lib/cape_versions.sh`); everything else is **mainnet**. The mainnet chain renders as before with `delta_prev` / `delta_first` toggle. The latest preview version (if any) appears as a single "sneak peek" column on the right with a single `delta_vs_latest_mainnet` figure — the baseline is the latest mainnet column for that scenario, **not** the previous overall column. Visually the column is tinted (`#fffbeb`) and tagged with a `preview` badge so it reads as different territory; the mainnet baseline toggle does not affect it.

### Default-variant only

Per-variant sections are dropped. Variants like `_builtincasing`, `_vanrossem`, `_prepacked` are alternative implementation strategies (sibling experiments answering "what if we wrote this differently?"), not points on a version timeline answering "how did the compiler evolve?". They remain visible in the per-scenario production report, which is the right place to compare implementations against each other. The evolution view filters `.variant == "default"` at the top of the jq pipeline.

### Why this overrides the original design

Option 2 in "Considered Options" treated production and preview as undifferentiated points on a timeline, on the grounds that "production / preview is just a `min_plutus_version` attribute on each version, not a top-level partition". That symmetry argument turned out to be wrong in practice for one specific reason: preview submissions are compiled against a different cost model, and a delta between a mainnet row and a preview row mixes both compiler-level improvement and cost-model drift in the same number. There is no clean way to interpret such a delta. Separating the two tracks restores the property that every delta in the mainnet chain is attributable to a compiler change.

### Data-model changes

- `scripts/cape-subcommands/submission/aggregate.sh` now emits `min_plutus_version` as CSV column 21. The value already existed in metadata (`compilation_config.min_plutus_version`) and was read internally for the `--target=preview/current` filter; it is now also exposed downstream.
- `build_evolution_stats` in `scripts/cape-subcommands/submission/report.sh` produces a flatter shape:
  - `.compilers[]` no longer carries a `variants[]` wrapper.
  - Each compiler has `mainnet_versions: [string]` and `preview_version: string | null` (the latest preview by version-key, or null).
  - Each scenario has `mainnet_values: [{version, cpu_units, …, delta_prev, delta_first}]` and `preview_value: {version, cpu_units, …, delta_vs_latest_mainnet} | null`.
- A scenario contributes to the timeline only when it has ≥2 non-null **mainnet** values. Preview-only scenarios carry no improvement signal and are dropped; a preview column without a mainnet baseline cannot produce a delta anyway.

### Out of scope for the refinement

- Promotion of preview submissions to mainnet when `CAPE_CURRENT_PLUTUS_VERSION` bumps. The classification re-derives on every render from current state — no migration is needed, the same submission simply shifts tracks the next time the report regenerates.
- A "Variants" report. Variants stay in the per-scenario production report. A dedicated variant-evolution view becomes interesting only when ≥2 variants share a version timeline; today none do.

## Refinement 2 (2026-06-22)

The first refinement classified a submission's track from `compilation_config.min_plutus_version`: a submission was preview when its required runtime exceeded `CAPE_CURRENT_PLUTUS_VERSION`. In practice this turned out to be brittle:

- Plinth_Unisay had four submissions on the `default` path (`Plinth_1.45.0.0_Unisay`, `Plinth_1.61.0.0_Unisay`, `Plinth_1.64.0.0_Unisay`, `Plinth_1.65.0.0_Unisay`) but only one of them — 1.61 — was authored with `min_plutus_version: "1.61.0.0"`. 1.64 and 1.65 default ship UPLC compatible with the mainnet cost model, so their metadata leaves `min_plutus_version` unset. The accidental result: the preview teaser column showed Plinth 1.61, not the most recent preview compilation.
- The author's actual intent for a given submission is "is this an alternative mainnet artifact or a preview-only artifact?". Reading that intent off a runtime version number is indirect; reading it off the directory name is explicit.

### New convention: explicit `_preview` variant suffix

Per `(scenario, compiler, version, author)` the author may now ship two submissions:

- `<Compiler>_<Version>_<Handle>/` — variant `default`, the mainnet artifact (no `min_plutus_version` required).
- `<Compiler>_<Version>_<Handle>_preview/` — variant `preview`, the preview artifact (typically with `min_plutus_version` set, executed via `cape submission measure --preview`).

Track classification in `build_evolution_stats` now reads directly from the variant suffix:

- mainnet timeline ← `.variant == "default"`
- preview teaser column ← `.variant == "preview"`

`min_plutus_version` retains its operational role: `cape submission verify --all` still skips preview submissions via `cape_is_preview_submission`, and `cape submission measure --preview` still consumes them. But it no longer steers the report.

### Rename actions taken in this refinement

- `Plinth_1.61.0.0_Unisay/` → `Plinth_1.61.0.0_Unisay_preview/` (7 scenarios). The 1.61 default release was always a preview submission — moving it onto the `_preview` path matches author intent.
- `Plinth_1.64.0.0_Unisay_builtincasing/` → `Plinth_1.64.0.0_Unisay_preview/` (7 scenarios). The `_builtincasing` label described the implementation strategy (BuiltinCasing extension) but failed to express the track-level intent that the submission targets preview runtime. The `_preview` suffix is the track-level statement; per-implementation experimentation belongs in additional variants (`_builtincasing`, `_vanrossem`, `_prepacked`, …) on either track.
- Two pre-existing filename mismatches were corrected along with the renames: `factorial_naive_recursion/.../factorial.uplc` → `factorial_naive_recursion.uplc`, `fibonacci_naive_recursion/.../fibonacci.uplc` → `fibonacci_naive_recursion.uplc`. These date to the pre-#203 dual naming convention; the variant rename was the natural occasion to normalize them.

### Why this is not the same as Option 3 ("variant in URL")

Option 3 in the original "Considered Options" treated each variant as its own page in the URL space. The current convention does the opposite: variant naming changes only the data-model classification (which timeline a submission belongs to), not the routing key — the page is still `compilers/<Compiler>_<Author>.html`. Two submissions `default` and `_preview` for the same `(compiler, version, author)` render as two columns on a single page, not two URLs.

### Consequences

- The mainnet timeline now expresses "how has the author's mainnet-targeted Plinth artifact evolved across releases?" rather than "all Plinth versions sorted, regardless of compatibility."
- The preview teaser shows the latest `_preview` submission for the author. After Plinth 1.65 preview submissions land (delegated separately), the teaser auto-promotes from 1.64 to 1.65.
- The naming convention is enforceable by humans (review-time check of directory names) without needing to inspect `metadata.json`.
- Backward break for anyone with `pr-*/evolution/.../Plinth_1.61.0.0_Unisay/...` bookmarks — the directory is gone. PR-preview deployments are short-lived; mainline pages refresh on the next CI run.

## Links

- Issue: [#204 — Redesign preview report as a per-compiler version-evolution view](https://github.com/IntersectMBO/UPLC-CAPE/issues/204)
- Origin PR: [#199 — Plinth 1.64 BuiltinCasing preview submissions](https://github.com/IntersectMBO/UPLC-CAPE/pull/199)
- Related: [ADR — PR Preview Deployment to GitHub Pages](20251022-pr-preview-deployment-to-github-pages.md) (unaffected; the per-PR `pr-<n>/` preview deployment mechanism is orthogonal to the report's internal `preview/` → `evolution/` rename)
