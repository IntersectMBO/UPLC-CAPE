# Scenario-Based Approach: Replacing Mode System with Dedicated Scenarios

- Status: accepted
- Date: 2025-10-09
- Tags: benchmarking, submissions, scenarios, simplification
- Supersedes: [20251006-baseopen-mode-system-with-submission-slugs.md](20251006-baseopen-mode-system-with-submission-slugs.md)

Technical Story: [Issue #59](https://github.com/IntersectMBO/UPLC-CAPE/issues/59), [PR #60](https://github.com/IntersectMBO/UPLC-CAPE/pull/60)

## Context and Problem Statement

The base/open mode system (implemented in PR #60) introduced a `mode` attribute and `slug` field to distinguish between prescribed algorithm benchmarks ("base" mode) and open optimization benchmarks ("open" mode) within the same scenario. After implementation and community feedback, several issues were identified:

**Problems with the Mode System:**

1. **Unnecessary Complexity**: The mode attribute added an extra dimension to track across metadata, CLI flags, directory naming, and reporting
2. **Verbose Documentation**: Scenario docs grew to 221-230 lines, including extensive mode-specific guidance that overwhelmed implementors
3. **Confusing Reporting**: HTML reports separated base/open modes within the same scenario, creating cognitive overhead
4. **Unclear Semantics**: The concept of "modes" within a single scenario was less intuitive than having separate scenarios

**Feedback Summary:**

- "The 'mode' as implemented adds too much complexity"
- "Scenario descriptions are too verbose for implementors"
- "Common submission instructions should be extracted"
- "Instead of mode=base, we should have dedicated scenarios like `fibonacci_naive_recursion`"

**Question**: How can we achieve the same goal (prescribed vs. open optimization benchmarks) with less complexity and better clarity?

## Decision Drivers

- Reduce cognitive overhead for users (implementors and report readers)
- Simplify metadata schema and CLI interface
- Make scenario intent immediately clear from directory structure
- Extract common guidance to avoid duplication
- Maintain backward compatibility path for existing submissions
- Enable future growth with additional constraint variations

## Considered Options

1. **Keep Mode System** - Retain base/open modes with incremental improvements
2. **Scenario-Based Approach** - Create dedicated scenarios for prescribed algorithms
3. **Tag-Based System** - Use metadata tags instead of mode field
4. **Hybrid Approach** - Combine scenarios with optional mode flags

## Decision Outcome

Chosen option: **"Scenario-Based Approach"**, because it:

- Eliminates the mode attribute entirely (simpler metadata and CLI)
- Makes intent immediately clear from scenario name (`fibonacci` vs `fibonacci_naive_recursion`)
- Reduces scenario documentation by 70-80% (from ~220 lines to ~40-60 lines)
- Enables natural growth (can add `fibonacci_iterative`, `fibonacci_memoized`, etc. as separate scenarios)
- Simplifies HTML reports (back to simple scenario-based organization)
- Follows principle of "make the common case simple" (most users want either prescribed OR open, rarely both)

### Implementation Overview

**New Scenario Structure:**

```
scenarios/
├── fibonacci/                    # Open optimization (any approach)
├── fibonacci_naive_recursion/    # Prescribed naive recursive algorithm
├── factorial/                    # Open optimization (any approach)
├── factorial_naive_recursion/    # Prescribed naive recursive algorithm
└── two-party-escrow/             # Open optimization only
```

**Submission Naming (Simplified):**

```
# Before (with mode system):
submissions/fibonacci/Plinth_1.45.0.0_Unisay_base/
submissions/fibonacci/Plinth_1.45.0.0_Unisay_open/
submissions/fibonacci/Plutarch_1.11.1_SeungheonOh_open_pfix/

# After (scenario-based):
submissions/fibonacci_naive_recursion/Plinth_1.45.0.0_Unisay/
submissions/fibonacci/Plinth_1.45.0.0_Unisay/
submissions/fibonacci/Plutarch_1.11.1_SeungheonOh_pfix/
```

**Streamlined Scenario Documentation:**

Scenario docs now contain ONLY (~40-60 lines):

- **Overview**: What this benchmark tests
- **Exact Task**: What to implement
- **Algorithm Specification**: Prescribed algorithm (for `_naive_recursion` variants) OR implementation freedom (for open scenarios)
- **Technical Constraints**: Plutus Core version, budget limits, etc.
- **Link to Common Guide**: Reference to `doc/submission-guide.md` for metrics, acceptance criteria, validation

**Extracted to `doc/submission-guide.md`:**

- General submission workflow
- Metrics explanation (CPU, memory, script size, term size)
- Acceptance criteria (correctness, budget compliance, determinism)
- Validation checklist and commands
- File requirements and templates

**Metadata Schema Simplification:**

```json
// Removed fields:
{
  "submission": {
    "mode": "base|open", // REMOVED
    "slug": "memoized" // REMOVED
  }
}

// Simplified to just scenario-based organization
// Variant suffixes optional in directory name only
```

**CLI Simplification:**

```bash
# Before (with mode):
cape submission new fibonacci Compiler 1.0 handle --mode base
cape submission new fibonacci Compiler 1.0 handle --mode open --slug memoized
cape submission list fibonacci --mode base
cape submission report fibonacci --mode open

# After (scenario-based):
cape submission new fibonacci_naive_recursion Compiler 1.0 handle
cape submission new fibonacci Compiler 1.0 handle
cape submission new fibonacci Compiler 1.0 handle_memoized  # variant in name
cape submission list fibonacci_naive_recursion
cape submission report fibonacci
```

**HTML Report Restoration:**

- Return to pre-mode format: single table per scenario
- `fibonacci` and `fibonacci_naive_recursion` are independent scenarios with separate reports
- No mode filtering or grouping logic
- Simpler, more predictable navigation

### Positive Consequences

- **70-80% reduction in scenario doc length**: From 221-230 lines to 40-60 lines
- **Eliminated CLI complexity**: No `--mode` or `--slug` flags to remember
- **Clearer intent**: Scenario name immediately indicates constraints
- **Simpler metadata**: Fewer required fields, less validation complexity
- **Better organization**: Common guidance in single location (`doc/submission-guide.md`)
- **Natural extensibility**: Easy to add `fibonacci_iterative`, `factorial_tail_recursive`, etc.
- **Simpler reports**: Back to straightforward scenario-based tables
- **Lower maintenance**: Less code to maintain (no mode filtering throughout)

### Negative Consequences

- **Breaking change**: Requires migration of existing submissions
- **More scenario directories**: `fibonacci_naive_recursion` adds directory (but clearer)
- **Potential proliferation**: Could lead to many scenario variants (but this is actually a feature for fine-grained benchmarking)
- **Migration effort**: All existing metadata.json files must be updated

## Pros and Cons of the Options

### Option 1: Keep Mode System

- Good, because no migration needed
- Good, because mode concept is familiar from compilation contexts
- Bad, because adds complexity to metadata, CLI, and reports
- Bad, because scenario docs remain too long (220+ lines)
- Bad, because mode semantics less clear than scenario names
- Bad, because feedback indicates it's "too much complexity"

### Option 2: Scenario-Based Approach (CHOSEN)

- Good, because scenario name immediately indicates intent
- Good, because massive documentation simplification (70-80% reduction)
- Good, because eliminates mode attribute entirely
- Good, because simpler CLI and metadata schema
- Good, because enables natural growth (add scenarios as needed)
- Good, because addresses all feedback points
- Bad, because requires migration of existing submissions
- Bad, because creates more top-level scenario directories

### Option 3: Tag-Based System

- Good, because flexible tagging without directory changes
- Bad, because tags hidden in metadata (less discoverable)
- Bad, because doesn't solve documentation verbosity
- Bad, because still requires attribute in metadata
- Bad, because unclear semantics (what tags mean what?)

### Option 4: Hybrid Approach

- Good, because could combine benefits
- Bad, because adds even more complexity
- Bad, because doesn't address core feedback about simplification
- Bad, because would still have verbose documentation

## Implementation Details

### Scenario Creation

New scenarios created with prescribed algorithms:

- `scenarios/fibonacci_naive_recursion/` - Naive recursive fibonacci implementation required
- `scenarios/factorial_naive_recursion/` - Naive recursive factorial implementation required

Each includes:

- Streamlined `.md` spec (~40-60 lines)
- `cape-tests.json` for verification
- Same structure as existing scenarios

### Documentation Extraction

Create `doc/submission-guide.md` containing:

- **Metrics Explanation**: What CPU units, memory units, script size, term size mean
- **Acceptance Criteria**: Correctness, budget compliance, determinism, self-contained, valid format
- **Validation Workflow**: Commands to verify and measure submissions
- **File Requirements**: What files are needed (uplc, metadata.json, metrics.json, README.md)
- **Templates**: Links to schema and template files
- **Common Checklist**: Steps all submissions should follow

All scenario docs link to this guide instead of duplicating content.

### Migration Strategy

**Existing Submissions:**

1. Create new scenario directories:
   - `submissions/fibonacci_naive_recursion/`
   - `submissions/factorial_naive_recursion/`

2. Migrate base mode submissions:
   - `fibonacci/*_base/` → `fibonacci_naive_recursion/*/` (remove `_base` suffix)
   - `factorial/*_base/` → `factorial_naive_recursion/*/` (remove `_base` suffix)

3. Migrate open mode submissions:
   - `fibonacci/*_open/` → `fibonacci/*/` (remove `_open` suffix)
   - `factorial/*_open/` → `factorial/*/` (remove `_open` suffix)
   - `factorial/*_open_pfix/` → `factorial/*_pfix/` (remove `_open_`, keep variant)

4. Update all `metadata.json` files:
   - Remove `"mode": "base|open"` field
   - Remove `"slug": "..."` field (if present)

**CLI Updates:**

Remove from all affected scripts:

- `--mode base|open` flag and associated logic
- `--slug <name>` flag and associated logic
- Mode-based filtering in list/report/aggregate commands
- Mode-based directory naming logic

**Schema Updates:**

Remove from `submissions/TEMPLATE/metadata.schema.json`:

- `submission.mode` field (was required)
- `submission.slug` field (was optional)

Update `metadata-template.json` accordingly.

**Report Generation:**

Restore pre-mode report format:

- Remove mode filtering/grouping
- Each scenario gets its own independent report
- `fibonacci` and `fibonacci_naive_recursion` treated as separate scenarios
- Simple scenario-based navigation

### Verification Strategy

**Scenario Compliance:**

For prescribed scenarios (`_naive_recursion`):

- Community review during PR process
- Submission README explains algorithm compliance
- Reviewers verify against scenario specification
- Honor system with clear guidelines

For open scenarios:

- No algorithmic constraints
- Only correctness verification via `cape-tests.json`
- Variant suffix (e.g., `_memoized`) for documentation only

## Applicability

**When to Use Prescribed Scenarios:**

- Synthetic benchmarks where algorithm can be clearly specified
- Compiler comparison needs (isolate compiler effectiveness)
- Educational purposes (demonstrate optimization of specific algorithm)

**When to Use Open Scenarios:**

- Real-world scenarios (complex validators)
- Optimization showcases (best achievable performance)
- Exploring different algorithmic approaches

**Growth Pattern:**

- Start with open scenario (e.g., `fibonacci`)
- Add prescribed variants as needed (e.g., `fibonacci_naive_recursion`, `fibonacci_iterative`)
- Each scenario is self-contained and independently useful

## Migration Timeline

1. **Phase 1**: Create new scenarios and documentation (fibonacci_naive_recursion, factorial_naive_recursion, submission-guide.md)
2. **Phase 2**: Remove mode system from CLI and schemas
3. **Phase 3**: Migrate existing submissions
4. **Phase 4**: Restore HTML report format
5. **Phase 5**: Update tests and project documentation
6. **Phase 6**: Final verification and testing

Estimated total effort: 20-26 hours

## Links

- [GitHub Issue #59](https://github.com/IntersectMBO/UPLC-CAPE/issues/59) - Original feature request for base/open mode system
- [GitHub PR #60](https://github.com/IntersectMBO/UPLC-CAPE/pull/60) - Implementation of mode system (now superseded)
- Supersedes: [ADR 20251006: Base/Open Mode System](20251006-baseopen-mode-system-with-submission-slugs.md)
