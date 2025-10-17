# UPLC Pretty-Printing Integration

- Status: accepted
- Date: 2025-10-17
- Tags: tooling, formatting, uplc, developer-experience

## Context and Problem Statement

UPLC files (`.uplc`) in the repository are often minified or inconsistently formatted, making them difficult to review, compare, and debug. The `plutus` executable from the Plutus repository provides pretty-printing capabilities via its output flag (`-o`), but there was no convenient way to:

1. Pretty-print UPLC files in place
2. Automatically format UPLC files as part of the development workflow
3. Ensure consistent formatting across all submissions

**Question**: How can we integrate UPLC pretty-printing into our existing tooling and workflow?

## Decision Drivers

- Maintain consistency with existing formatting tools (treefmt)
- Provide both manual and automatic formatting options
- Avoid breaking existing workflows
- Reuse existing infrastructure (plutus executable, treefmt)
- Follow naming conventions consistent with project structure

## Considered Options

1. **Manual plutus invocation only** - Document the plutus command pattern
2. **Standalone script only** - Create a helper script without treefmt integration
3. **Full treefmt integration** - Add UPLC formatting to treefmt with helper scripts
4. **Custom Haskell tool** - Build a dedicated UPLC formatter in the measure-app

## Decision Outcome

Chosen option: **"Full treefmt integration"**, because it:

- Integrates seamlessly with existing `treefmt` workflow
- Provides both manual (`pretty-uplc`) and automatic (via `treefmt`) formatting
- Requires minimal additional tooling (reuses existing `plutus` executable)
- Follows established patterns (similar to how fourmolu handles Haskell)
- Consistent naming with existing script (`scripts/pretty-uplc.sh`)

### Implementation Overview

**Components Created:**

1. **`scripts/pretty-uplc.sh`**: Standalone bash script for manual use
   - Takes a single UPLC file path as argument
   - Uses temporary file for safe in-place replacement
   - Validates file existence and plutus availability
   - Provides clear error messages and success confirmation

2. **`pretty-uplc` command**: Nix shell wrapper (defined in `flake.nix`)
   - Available after `nix develop` or `direnv reload`
   - Provides convenient command-line interface
   - Identical implementation to standalone script

3. **treefmt integration**: Added to `treefmt.toml`
   - Formatter: `pretty-uplc`
   - Command: `scripts/pretty-uplc.sh`
   - Includes: `*.uplc`
   - Runs automatically with `treefmt`

**Usage Patterns:**

```bash
# Manual single file formatting
pretty-uplc submissions/fibonacci/MyCompiler_1.0.0_handle/fibonacci.uplc

# Format all files via treefmt (includes UPLC files)
treefmt

# Direct script invocation (without nix shell command)
./scripts/pretty-uplc.sh path/to/file.uplc
```

**Technical Implementation:**

The script uses a safe temporary file pattern to avoid data loss:

```bash
temp_file=$(mktemp)
trap 'rm -f "$temp_file"' EXIT
plutus "$uplc_file" -o "$temp_file"
# Ensure file ends with a newline
if [ -s "$temp_file" ] && [ "$(tail -c 1 "$temp_file" | wc -l)" -eq 0 ]; then
  echo "" >> "$temp_file"
fi
mv "$temp_file" "$uplc_file"
```

This ensures:

- No data loss if plutus fails
- Automatic cleanup of temporary files
- Atomic replacement (mv is atomic on same filesystem)
- Files always end with a newline (POSIX standard)

### Positive Consequences

- **Consistent formatting**: All UPLC files follow the same pretty-printed format
- **Improved readability**: Pretty-printed UPLC easier to review in PRs
- **Automated workflow**: UPLC files formatted automatically with `treefmt`
- **Manual control**: Can format individual files when needed
- **Zero dependencies**: Reuses existing `plutus` executable from development environment
- **Safe operation**: Temporary file pattern prevents data loss
- **Clear feedback**: Success/error messages guide users

### Negative Consequences

- **Additional processing time**: treefmt now processes UPLC files (16 files currently)
- **Requires plutus**: Only works inside nix shell (documented limitation)
- **Line length changes**: Pretty-printing may increase file size/lines
- **Git diffs**: Initial formatting of existing files creates large diffs

## Documentation Updates

Updated the following documentation:

1. **CLAUDE.md**:
   - Added `pretty-uplc` to tools list
   - Added UPLC formatting command reference
   - Updated mandatory treefmt file types to include `.uplc`

2. **README.md**:
   - Added UPLC formatting section under "Development"
   - Documented both manual and automatic usage
   - Explained integration with treefmt

3. **This ADR**: Documents decision rationale and implementation

## Applicability

**When to Use:**

- Before committing UPLC files to ensure consistent formatting
- When reviewing UPLC files for debugging
- As part of automated CI/CD formatting checks
- When comparing UPLC outputs from different compilers

**Limitations:**

- Only works inside nix development shell (requires `plutus` executable)
- Formats entire file (no selective formatting)
- Requires write access to UPLC file location

## Links

- [Plutus Repository](https://github.com/IntersectMBO/plutus) - Source of `plutus` executable
- [treefmt](https://treefmt.com/) - Multi-formatter orchestration tool
- Related file: `scripts/pretty-uplc.sh`
- Related file: `treefmt.toml`
- Related file: `flake.nix` (pretty-uplc command definition)
