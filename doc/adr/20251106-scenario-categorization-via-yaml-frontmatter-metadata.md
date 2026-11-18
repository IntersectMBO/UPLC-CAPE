# Scenario Categorization via YAML Frontmatter Metadata

- Status: accepted
- Date: 2026-11-18
- Tags: scenarios, metadata, categorization, reporting

Technical Story: [Issue #137](https://github.com/IntersectMBO/UPLC-CAPE/issues/137), revised in [PR #278](https://github.com/IntersectMBO/UPLC-CAPE/pull/278)

## Context and Problem Statement

The HTML report landing page needs to clearly distinguish between two kinds of benchmark scenarios: scenarios that prescribe an algorithm, so that only compiler optimizations differ, and scenarios that allow any approach. Scenarios need an explicit categorization mechanism that works for all cases (e.g., `ecd` prescribes its algorithm, while `fibonacci` allows any approach).

The report later also separated smart contracts from computational tasks. This second distinction is independent of the first: `fibonacci` is a computational task that allows any approach, and `htlc` is a smart contract that allows any approach.

**Question**: How should scenarios be categorized in a way that is explicit, maintainable, and supports the reporting system's need to separate them?

## Decision Drivers

- Need explicit, declarative categorization that doesn't rely on implicit naming patterns
- Must be easy for scenario authors to specify and understand
- Should be maintainable and validate-able
- Must support the HTML report generation system
- Should be embedded in scenario specification (single source of truth)
- Must provide clear error messages when misconfigured

## Considered Options

1. **YAML Frontmatter Metadata** - Add attributes to scenario markdown files
2. **Separate Metadata File** - Create `metadata.json` alongside scenario specification
3. **Directory Structure** - Use subdirectories like `scenarios/fixed/` and `scenarios/open/`
4. **Naming Convention** - Use suffixes like `_naive_recursion` to infer category

## Decision Outcome

Chosen option: **"YAML Frontmatter Metadata"**, because it provides the best balance of explicitness, maintainability, and integration with existing documentation structure.

### Implementation

Add YAML frontmatter with two independent attributes to the beginning of each scenario specification file (`scenarios/{name}/{name}.md`):

```yaml
---
optimization: compiler-only # or "unrestricted"
workload: synthetic # or "real-world"
---
```

**`optimization` values**:

- `compiler-only`: The scenario prescribes an algorithm that all submissions must follow, so only compiler optimizations differ (e.g., `ecd`, `factorial_naive_recursion`, `fibonacci_naive_recursion`)
- `unrestricted`: Submissions may use any implementation approach (e.g., `factorial`, `fibonacci`, `two_party_escrow`)

**`workload` values**:

- `synthetic`: A computational task, such as a recursive function of an integer (e.g., `ecd`, `fibonacci`)
- `real-world`: A smart contract that validates a transaction (e.g., `htlc`, `linear_vesting`, `two_party_escrow`)

The report groups scenarios by `workload` in tabs and shows `optimization` on each scenario card.

**Extraction Method**: The `scenario_attribute()` function in `report.sh` reads one attribute with AWK and checks it against the allowed values:

```bash
optimization=$(scenario_attribute "$benchmark" optimization compiler-only unrestricted)
workload=$(scenario_attribute "$benchmark" workload synthetic real-world)
```

**Error Handling**:

- If scenario file is missing: Error with clear message
- If an attribute is missing or has an invalid value: Error naming the attribute, its allowed values and the file

### Revision (2026-11-18)

The original decision used one attribute, `category: fixed | open`. The revision renames it and adds a second attribute:

- `category: fixed` became `optimization: compiler-only`, and `category: open` became `optimization: unrestricted`. The word "category" did not say what it classified, and "fixed" did not say what was fixed. In these scenarios the source algorithm is fixed, while compiler optimizations are exactly what the benchmark compares.
- `workload: synthetic | real-world` is new. "Synthetic workload" is the established benchmarking term for a constructed task, as opposed to a real one. Grouping by `optimization` alone would have put `fibonacci` and `factorial` next to the smart contracts.

### Positive Consequences

- ✅ Explicit categorization visible at the top of scenario specification
- ✅ Single source of truth (embedded in the spec that defines the scenario)
- ✅ Easy for scenario authors to understand and specify
- ✅ Self-documenting (the attributes are part of the specification)
- ✅ Supports validation and error checking
- ✅ No additional files or complex directory restructuring required
- ✅ Standard YAML format familiar to developers
- ✅ Clear error messages guide users to fix issues

### Negative Consequences

- Requires updating all existing scenario files to add frontmatter
- Adds a small amount of boilerplate to scenario specifications
- Requires parsing YAML frontmatter in bash scripts (using AWK)

## Pros and Cons of the Options

### Option 1: YAML Frontmatter Metadata (CHOSEN)

- Good, because it's explicit and declarative
- Good, because it's embedded in the scenario specification (single source of truth)
- Good, because YAML frontmatter is a standard pattern in documentation
- Good, because it's self-documenting
- Good, because it supports validation
- Bad, because it requires updating existing files
- Bad, because it adds boilerplate to scenario specs

### Option 2: Separate Metadata File

- Good, because it separates concerns (spec vs metadata)
- Good, because it could support more extensive metadata
- Bad, because it creates multiple files to maintain per scenario
- Bad, because it fragments the source of truth
- Bad, because scenario authors must remember to update both files

### Option 3: Directory Structure

- Good, because categorization is visible in file paths
- Bad, because it requires major directory restructuring
- Bad, because it complicates scenario naming and references
- Bad, because moving scenarios between categories is harder
- Bad, because two independent attributes would need nested directories

### Option 4: Naming Convention

- Good, because no additional files or metadata required
- Good, because categorization is visible in the scenario name
- Bad, because it's implicit and error-prone
- Bad, because it doesn't cover all cases (e.g., `ecd` doesn't have a distinguishing suffix)
- Bad, because it couples scenario naming to categorization logic
- Bad, because scenarios might not follow the pattern consistently

## Migration

All scenario files declare both attributes:

| Scenario                    | `optimization`  | `workload`   |
| --------------------------- | --------------- | ------------ |
| `ecd`                       | `compiler-only` | `synthetic`  |
| `factorial_naive_recursion` | `compiler-only` | `synthetic`  |
| `fibonacci_naive_recursion` | `compiler-only` | `synthetic`  |
| `factorial`                 | `unrestricted`  | `synthetic`  |
| `fibonacci`                 | `unrestricted`  | `synthetic`  |
| `htlc`                      | `unrestricted`  | `real-world` |
| `linear_vesting`            | `unrestricted`  | `real-world` |
| `two_party_escrow`          | `unrestricted`  | `real-world` |

## Links

- [Issue #137: Improve HTML report landing page](https://github.com/IntersectMBO/UPLC-CAPE/issues/137)
- [PR #278: report: make scenario rankings selectable](https://github.com/IntersectMBO/UPLC-CAPE/pull/278)
- Related: [ADR 20251009: Scenario-Based Approach](20251009-scenario-based-approach-replacing-mode-system.md) - Defines fixed vs open scenarios
