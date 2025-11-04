# Base/Open Mode System with Submission Slugs

- Status: superseded by [20251009-scenario-based-approach-replacing-mode-system](20251009-scenario-based-approach-replacing-mode-system.md)
- Date: 2025-10-06
- Tags: benchmarking, submissions, evaluation-modes, compiler-comparison

Technical Story: [Issue #59](https://github.com/IntersectMBO/UPLC-CAPE/issues/59)

> **Note**: This ADR describes the base/open mode system that was implemented in PR #60 but subsequently replaced by a scenario-based approach (see superseding ADR). The mode system added unnecessary complexity; the scenario-based approach achieves the same goals more simply.

## Context and Problem Statement

UPLC-CAPE users have two distinct benchmarking needs that are sometimes in conflict:

1. **Compiler Comparison** (Group A): Users want to compare compiler optimization effectiveness in isolation by having all submissions implement the **same algorithm**. This provides apples-to-apples comparison where performance differences reflect compiler capabilities rather than algorithmic choices.

2. **Real-World Competition** (Group B): Users want submissions to showcase ecosystem capabilities using **any optimizations or algorithms** (memoization, metaprogramming, different algorithms). This reflects practical deployment strategies and compiler USPs.

Currently, the framework allows any implementation approach, but all submissions compete in a single pool. This makes it impossible to isolate compiler effectiveness from algorithmic choices, frustrating both user groups.

**Question**: How can we support both pure compiler comparison and open optimization competition within the same framework?

## Decision Drivers

- Must satisfy both user groups without forcing them into a single evaluation mode
- Should enable multiple optimization experiments per compiler/author
- Must maintain backward compatibility with existing submissions
- Should provide clear, self-documenting organization
- Must be intuitive for new users while supporting advanced workflows
- Should facilitate both academic research and practical developer guidance

## Considered Options

1. **Separate Scenario Types** - Create duplicate scenarios (e.g., "fibonacci-constrained" vs "fibonacci-open")
2. **Implementation Tracks** - Add track/category system within scenarios
3. **Base/Open Mode System with Slugs** - Two evaluation modes with flexible slug identifiers
4. **Tag-Based System** - Use metadata tags for algorithmic constraints

## Decision Outcome

Chosen option: **"Base/Open Mode System with Slugs"**, because it:

- Provides clear semantic distinction between evaluation contexts ("base" = prescribed algorithm, "open" = any optimization)
- Enables multiple experiments per compiler via descriptive slugs
- Maintains single scenario definition (no duplication)
- Scales to any number of optimization variants
- Is backward compatible (existing submissions → "open_default")
- Uses familiar terminology ("mode" aligns with compilation contexts)

### Implementation Overview

**Two Evaluation Modes:**

1. **"base" mode**: Implements exact algorithm prescribed in scenario specification
   - One submission per compiler/author
   - Pure compiler comparison (apples-to-apples)
   - No slug needed

2. **"open" mode**: Any algorithm or optimization technique allowed
   - Multiple submissions per compiler/author
   - Optional slugs for specific optimizations: "memoized", "unrolled", "pfix", etc.
   - Slug can be omitted for generic/default implementation
   - Real-world competition

**Directory Naming:**

```
submissions/{scenario}/{Compiler}_{Version}_{Author}_base/
submissions/{scenario}/{Compiler}_{Version}_{Author}_open/            # generic/default
submissions/{scenario}/{Compiler}_{Version}_{Author}_open_{slug}/    # specific optimization
```

**Slug Philosophy:**

- **Omit slug**: For generic/default open mode implementations (most common)
- **Add slug**: Only when submitting specific optimization variants (e.g., "memoized", "tail-recursive")
- **Benefit**: Cleaner naming, slug only adds information when needed

**CLI Default Behavior:**

- Omitting `--mode` shows **all modes** (most comprehensive view)
- Explicit `--mode base` or `--mode open` filters to specific mode

### Positive Consequences

- Both user groups get dedicated evaluation contexts without compromise
- Compiler authors can demonstrate both compliance (base) and innovation (open)
- Reports clearly separate algorithmic comparison from optimization competition
- Multiple optimization experiments trackable via descriptive slugs
- Framework becomes more valuable for academic research AND practical guidance
- Backward compatible: existing submissions continue working as "open_default"

### Negative Consequences

- Increased complexity in submission directory structure
- Base mode compliance relies on community review (honor system)
- Scenario authors must define base mode algorithm specification
- More options for users to understand (though with good defaults)

## Pros and Cons of the Options

### Option 1: Separate Scenario Types

Create duplicate scenarios (e.g., "fibonacci-constrained" vs "fibonacci-open")

- Good, because conceptually simple (one scenario = one evaluation mode)
- Good, because no changes to existing submission structure
- Bad, because duplicates scenario specifications (maintenance burden)
- Bad, because fragments community efforts across duplicate scenarios
- Bad, because doesn't scale to multiple optimization variants

### Option 2: Implementation Tracks

Add track/category system within scenarios

- Good, because keeps scenarios unified
- Good, because supports multiple tracks per scenario
- Bad, because "track" terminology less clear than "mode"
- Bad, because doesn't directly convey algorithmic constraint concept
- Bad, because requires additional slug system for variants anyway

### Option 3: Base/Open Mode System with Slugs (CHOSEN)

Two evaluation modes with flexible slug system

- Good, because clear semantic distinction ("base" = prescribed, "open" = free)
- Good, because slug system enables unlimited optimization variants
- Good, because "mode" terminology familiar from compilation contexts
- Good, because backward compatible
- Good, because self-documenting (slug describes optimization)
- Good, because comprehensive default (omitting --mode shows all)
- Bad, because base mode compliance relies on community review
- Bad, because adds complexity to directory naming

### Option 4: Tag-Based System

Use metadata tags for algorithmic constraints

- Good, because flexible tagging system
- Good, because no directory structure changes
- Bad, because less discoverable (hidden in metadata)
- Bad, because tags less structured than explicit modes
- Bad, because unclear semantics (what tags mean what?)
- Bad, because makes filtering and reporting more complex

## Implementation Details

### Scenario Definition Enhancement

Scenarios gain optional base mode algorithm specification:

```markdown
## Base Mode Algorithm

All "base" mode submissions MUST implement this exact algorithm:

\`\`\`haskell fibonacci :: Integer -> Integer fibonacci n | n <= 1 = n | otherwise = fibonacci (n - 1) + fibonacci (n - 2) \`\`\`

**Requirements:**

- Naive recursive implementation (no memoization)
- No loop-based iteration
- Direct translation of mathematical definition
```

### Metadata Schema Addition

```json
{
  "submission": {
    "mode": "open", // Required: "base" or "open"
    "slug": "memoized", // Required for open, omitted for base
    "implementation_notes": "..."
  }
}
```

### CLI Enhancements

```bash
# Create submissions
cape submission new fibonacci Compiler 1.0 author --mode base
cape submission new fibonacci Compiler 1.0 author --mode open --slug memoized

# List submissions (default: all modes)
cape submission list fibonacci
cape submission list fibonacci --mode base

# Generate reports (default: all modes)
cape submission report fibonacci
cape submission report fibonacci --mode base
```

### Verification Strategy

**Base Mode**: Community review during PR process

- Submission README explains algorithm compliance
- Reviewers verify against scenario specification
- Honor system with clear guidelines

**Open Mode**: Existing correctness verification only

- No algorithmic constraints
- Slug for identification/documentation only

## Applicability

**Synthetic Scenarios** (fibonacci, factorial):

- Both modes applicable
- Clear algorithmic prescriptions for base mode
- Room for optimization exploration in open mode

**Real-world Scenarios** (two_party_escrow):

- Typically only open mode
- Complex validators lack clear "base" algorithm
- Focus on practical deployment strategies

Scenarios can optionally define base mode. If undefined, only open mode available.

## Links

- [GitHub Issue #59](https://github.com/IntersectMBO/UPLC-CAPE/issues/59) - Full specification and implementation plan
- Related ADR: [Multi-View Approach for Benchmark Scenario Specification](0003-use-multi-view-approach-for-benchmark-scenarios.md)
