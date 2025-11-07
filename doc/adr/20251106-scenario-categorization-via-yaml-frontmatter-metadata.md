# Scenario Categorization via YAML Frontmatter Metadata

- Status: accepted
- Date: 2025-11-06
- Tags: scenarios, metadata, categorization, reporting

Technical Story: [Issue #137](https://github.com/IntersectMBO/UPLC-CAPE/issues/137)

## Context and Problem Statement

The HTML report landing page needs to clearly distinguish between two types of benchmark scenarios: fixed algorithm scenarios (prescribed implementations for compiler comparison) and open optimization scenarios (any approach allowed). Scenarios need an explicit categorization mechanism that works for all cases (e.g., `ecd` is a fixed algorithm scenario, while `fibonacci` is open optimization).

**Question**: How should scenarios be categorized in a way that is explicit, maintainable, and supports the reporting system's need to separate fixed vs open scenarios?

## Decision Drivers

- Need explicit, declarative categorization that doesn't rely on implicit naming patterns
- Must be easy for scenario authors to specify and understand
- Should be maintainable and validate-able
- Must support the HTML report generation system
- Should be embedded in scenario specification (single source of truth)
- Must provide clear error messages when misconfigured

## Considered Options

1. **YAML Frontmatter Metadata** - Add `category` attribute to scenario markdown files
2. **Separate Metadata File** - Create `metadata.json` alongside scenario specification
3. **Directory Structure** - Use subdirectories like `scenarios/fixed/` and `scenarios/open/`
4. **Naming Convention** - Use suffixes like `_naive_recursion` to infer category

## Decision Outcome

Chosen option: **"YAML Frontmatter Metadata"**, because it provides the best balance of explicitness, maintainability, and integration with existing documentation structure.

### Implementation

Add YAML frontmatter to the beginning of each scenario specification file (`scenarios/{name}/{name}.md`):

```yaml
---
category: fixed # or "open"
---
```

**Category Values**:

- `fixed`: Fixed algorithm scenarios requiring prescribed implementation (e.g., `ecd`, `factorial_naive_recursion`, `fibonacci_naive_recursion`)
- `open`: Open optimization scenarios allowing any approach (e.g., `factorial`, `fibonacci`, `two_party_escrow`)

**Extraction Method**: The `categorize_scenario()` function in `report.sh` extracts the category using AWK:

```bash
category=$(awk 'BEGIN{in_fm=0} /^---$/ {in_fm++; next} in_fm==1 && /^category:/ {print $2; exit}' "$scenario_file")
```

**Error Handling**:

- If scenario file is missing: Error with clear message
- If category attribute is missing: Error with instructions to add frontmatter
- If category value is invalid (not "fixed" or "open"): Error with valid values

### Positive Consequences

- ✅ Explicit categorization visible at the top of scenario specification
- ✅ Single source of truth (embedded in the spec that defines the scenario)
- ✅ Easy for scenario authors to understand and specify
- ✅ Self-documenting (category is part of the specification)
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

### Option 4: Naming Convention

- Good, because no additional files or metadata required
- Good, because categorization is visible in the scenario name
- Bad, because it's implicit and error-prone
- Bad, because it doesn't cover all cases (e.g., `ecd` doesn't have a distinguishing suffix)
- Bad, because it couples scenario naming to categorization logic
- Bad, because scenarios might not follow the pattern consistently

## Migration

All existing scenario files were updated to include category frontmatter:

**Fixed algorithm scenarios**:

- `scenarios/ecd/ecd.md`: `category: fixed`
- `scenarios/factorial_naive_recursion/factorial_naive_recursion.md`: `category: fixed`
- `scenarios/fibonacci_naive_recursion/fibonacci_naive_recursion.md`: `category: fixed`

**Open optimization scenarios**:

- `scenarios/factorial/factorial.md`: `category: open`
- `scenarios/fibonacci/fibonacci.md`: `category: open`
- `scenarios/two_party_escrow/two_party_escrow.md`: `category: open`

## Links

- [Issue #137: Improve HTML report landing page](https://github.com/IntersectMBO/UPLC-CAPE/issues/137)
- Related: [ADR 20251009: Scenario-Based Approach](20251009-scenario-based-approach-replacing-mode-system.md) - Defines fixed vs open scenarios
