# Contributing to UPLC-CAPE

Thank you for your interest in contributing benchmarks to UPLC-CAPE. This guide explains how to submit your compiled UPLC program and associated metrics.

For the full, up-to-date CLI reference and workflows, see USAGE.md at the project root.

## Directory Structure for Submissions

Each scenario has its own folder under `submissions/`. Inside `submissions/<benchmark>/`, create a submission folder using the standardized naming convention:

**Folder Name Template**: `<Language>_<Version>_<GitHubHandle>`

Examples:

- `Aiken_1.1.17_KtorZ`
- `Plinth_1.52.0.0_Unisay`

The submission folder **must** contain:

1. **`<scenario>.uplc`**
   - The fully-applied UPLC program with the benchmark target baked in (e.g. `fibonacci.uplc`).

2. **`metrics.json`**
   - Performance measurements. Validate this file against `submissions/<template-name>/metrics.schema.json`.
   - The initial file is scaffolded from `submissions/<template-name>/metrics-template.json`.

3. **`metadata.json`**
   - Compiler metadata. Validate this file against `submissions/<template-name>/metadata.schema.json`.
   - The initial file is scaffolded from `submissions/<template-name>/metadata-template.json`.
   - For baseline scenarios, use `"compiler": { "name": "Plinth", ... }`.

4. _(Optional)_ **`source/`**
   - Original source code (e.g. `fibonacci.plinth`) if you choose to publish it.

5. _(Optional)_ **`config.json`**
   - Compilation parameters (flags, optimization levels) that affect the generated UPLC.

6. **`README.md`**
   - Implementation notes using the template from `submissions/<template-name>/benchmark-README-template.md`.

> Note: Templates are starting points for files you edit. Schemas are used by tooling to validate your JSON files.

## Submission Process

All submissions start with the same basic setup:

1. **Fork this repository** and clone your fork.

### Recommended Approach (Nix Integration)

The recommended (though not mandatory) approach for contributing implementations:

1. **Add required compiler pipeline and tools to the nix shell** by modifying `flake.nix` to include your compiler's dependencies.
2. **Use available compiler in the shell** to write and evaluate a benchmark implementation:

   ```bash
   nix develop
   # Your compiler tools are now available in the shell
   # Write, compile, and test your implementation
   ```

3. **Prepare the submission as a PR**, honoring the submission structure and producing all expected files (metrics, source, etc.).

### Standard Process

For those who prefer not to modify the nix environment:

1. Use the provided initialization script to create your submission folder:

   ```bash
   nix develop  # Enter the development environment
   cape submission new <scenario> <language> <version> <github-handle>
   # Example: cape submission new fibonacci Aiken 1.1.17 KtorZ
   ```

   This will create: `submissions/<benchmark>/<Language>_<Version>_<GitHubHandle>/`

2. Copy your UPLC program and fill in the generated template files:

   ```bash
   cd submissions/<benchmark>/<Language>_<Version>_<GitHubHandle>
   cp path/to/your-program.uplc <scenario>.uplc
   # Edit metrics.json, metadata.json, and README.md with your data
   ```

3. **Verify your submission** before committing:

   ```bash
   # Verify correctness and validate schemas in your submission directory
   cape submission verify .

   # Or verify all submissions
   cape submission verify --all
   ```

4. (Optional) Add `source/` and `config.json` if you wish to share your source or compilation parameters.
5. Commit and push to your fork.
6. Open a pull request against the `main` branch.

## Creating New Benchmarks

If you want to propose a new benchmark scenario for the CAPE framework:

### 1. Create a Benchmark Specification

Use the benchmark creation tool to generate a template:

```bash
cape benchmark new <benchmark-name>
```

The benchmark name should be:

- Lowercase letters and hyphens only
- Descriptive of the computational task
- Examples: `fibonacci`, `two-party-escrow`, `dao-voting`, `merkle-proof`

### 2. Complete the Benchmark Definition

Edit `scenarios/<benchmark-name>.md` and replace all `{placeholder}` values with:

- **Overview**: Clear description of what the benchmark tests
- **Target Computation**: Specific computation with expected result
- **Technical Constraints**: Budget limits, determinism requirements
- **Performance Characteristics**: Expected ranges for metrics
- **Test Cases**: Validation criteria and edge cases
- **Implementation Notes**: Guidance and optimization opportunities

### 3. Benchmark Review Process

1. **Completeness Check**: Ensure all placeholders are replaced with concrete values
2. **Technical Review**: Verify the benchmark is implementable and measurable
3. **Community Feedback**: Share the benchmark specification for input
4. **Reference Implementation**: Create a Plinth baseline implementation

### 4. Benchmark Requirements

All benchmarks must define:

- A specific, deterministic computation with known expected result
- Clear success criteria for validation
- Fully-applied UPLC program constraints (no runtime parameters)
- Performance measurement guidelines
- Budget constraints that fit within CEK machine limits

## Templates and Examples

- **Metrics Template**: `submissions/TEMPLATE/metrics-template.json`
- **Metadata Template**: `submissions/TEMPLATE/metadata-template.json`
- **Metrics Schema**: `submissions/TEMPLATE/metrics.schema.json`
- **Metadata Schema**: `submissions/TEMPLATE/metadata.schema.json`
- **Benchmark README**: `submissions/TEMPLATE/benchmark-README-template.md`

## Quick Start

To quickly initialize a new submission:

```bash
# Enter the development environment
nix develop

# Initialize your submission folder
cape submission new fibonacci Aiken 1.1.17 KtorZ

# This creates: submissions/fibonacci/Aiken_1.1.17_KtorZ/
# with all required template files ready to fill in
```
