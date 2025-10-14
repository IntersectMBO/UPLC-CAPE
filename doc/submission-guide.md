# UPLC-CAPE Submission Guide

This guide provides common information for all UPLC-CAPE benchmark submissions, including metrics explanation, acceptance criteria, validation steps, and file requirements.

## Table of Contents

- [Metrics Explained](#metrics-explained)
- [Acceptance Criteria](#acceptance-criteria)
- [Submission Requirements](#submission-requirements)
- [Source Code Reproducibility](#source-code-reproducibility)
- [File Structure](#file-structure)
- [Validation Workflow](#validation-workflow)
- [Common Checklist](#common-checklist)

---

## Metrics Explained

All submissions are measured on these standardized metrics using the CEK machine evaluator:

| Metric | Description | Purpose | Measurement |
| --- | --- | --- | --- |
| **CPU Units** | Total execution units consumed during evaluation | Computational efficiency | CEK machine steps |
| **Memory Units** | Peak memory usage during execution | Memory efficiency | CEK machine memory |
| **Script Size** | Compiled UPLC script size in bytes | Code generation efficiency | Bytes (serialized) |
| **Term Size** | UPLC term representation size | Optimization effectiveness | AST nodes |

### Performance Aggregation Strategies

The `measure` tool runs multiple test cases per program and provides several aggregation methods for CPU and memory metrics:

- `maximum`: Peak resource usage across all test cases (worst-case performance)
- `sum`: Total computational work across all test cases (overall efficiency)
- `minimum`: Best-case resource usage (optimal performance)
- `median`: Typical resource usage (normal performance)
- `sum_positive`: Total resources for successful test cases only (valid execution cost)
- `sum_negative`: Total resources for failed test cases only (error handling cost)

Higher-level tooling can extract the most relevant aggregation for specific analysis needs.

**Measurement Environment**: Standard CEK machine evaluator with default budget limits.

---

## Acceptance Criteria

Your submission passes if it meets ALL of the following criteria:

- ✅ **Correctness**: Program produces the expected output for all test cases
  - For fully-applied programs: Reduces to the expected result
  - For parameterized programs: Passes all tests in the scenario's `cape-tests.json`
- ✅ **Budget Compliance**: Executes within CEK machine CPU and memory limits for all test cases
- ✅ **Determinism**: Produces identical results across multiple executions
- ✅ **Self-Contained**: No external dependencies or runtime parameters (unless scenario specifically allows parameterized programs)
- ✅ **File Format**: Valid UPLC program that can be executed by the CEK evaluator
- ✅ **Schema Compliance**: All JSON files validate against provided schemas

---

## Submission Requirements

### Required Files

Each submission must include:

1. **`{scenario}.uplc`** - Your compiled UPLC program
   - For fully-applied scenarios: The program should execute immediately to produce the result
   - For parameterized scenarios: The program should accept inputs defined in `cape-tests.json`
   - Must be valid UPLC that can be evaluated by the CEK machine

2. **`metadata.json`** - Compiler and contributor information
   - Must validate against `submissions/TEMPLATE/metadata.schema.json`
   - Include compiler name, version, and optional commit hash
   - Specify compilation configuration (optimization level, flags, dependencies)
   - Provide contributor information (optional, for privacy)
   - Include implementation notes explaining your approach

3. **`metrics.json`** - Performance measurements
   - Must validate against `submissions/TEMPLATE/metrics.schema.json`
   - Generated automatically by `cape submission measure`
   - Contains CPU units, memory units, script size, term size
   - Includes per-test-case evaluation results
   - Records execution environment and timestamp

4. **`README.md`** - Implementation description
   - Brief explanation of your approach
   - Algorithmic choices made
   - Any compiler-specific optimizations used
   - Known limitations or caveats
   - For prescribed algorithm scenarios: Explanation of how your implementation matches the required algorithm

5. **`source/`** (Optional) - Source code directory
   - Include if source code is publicly shareable
   - Helps reviewers verify correctness and algorithm compliance
   - Not required if proprietary or confidential

### Directory Naming Convention

Place your submission in:

```
submissions/{scenario}/{Compiler}_{Version}_{Handle}[_{variant}]/
```

**Examples:**

```
submissions/fibonacci/Plinth_1.45.0.0_Unisay/           # Generic/default
submissions/fibonacci/Plinth_1.45.0.0_Unisay_memoized/ # Optimization variant
submissions/fibonacci_naive_recursion/Plinth_1.45.0.0_Unisay/  # Prescribed algorithm
```

**Variant suffixes** (optional): Use when submitting multiple optimization approaches for the same scenario (e.g., `_memoized`, `_iterative`, `_pfix`).

---

## Source Code Reproducibility

Reproducibility is a core principle of UPLC-CAPE. Reviewers and future users must be able to verify that your UPLC program was compiled correctly from source code. This section explains how to provide source code in a reproducible manner.

### Why Reproducibility Matters

- **Verification**: Allows the community to verify UPLC output matches source compilation
- **Trust**: Builds confidence in benchmark results
- **Learning**: Helps others understand compilation techniques and optimizations
- **Future-proofing**: Ensures submissions remain verifiable over time

### Option 1: Inline Source (`source/` directory)

Include source files directly in your submission's `source/` directory.

**When to use:**

- Simple, self-contained programs
- Single-file implementations
- No complex build dependencies

**What to include:**

- All source files needed for compilation
- `README.md` with build instructions
- Any configuration files (e.g., `cabal.project`, `package.json`)

**Limitations:**

- May not capture full development environment
- Complex multi-project setups can be difficult to replicate
- Large codebases add unnecessary repository bloat

**Example structure:**

```
submissions/fibonacci/MyCompiler_1.0.0_myhandle/
├── fibonacci.uplc
├── metadata.json
├── metrics.json
├── README.md
└── source/
    ├── README.md           # Build instructions
    ├── Fibonacci.hs        # Source file
    └── cabal.project       # Build configuration
```

### Option 2: External Repository Reference (Recommended)

Reference a dedicated external repository with version pinning using git tags or commits.

**When to use:**

- Multi-file projects
- Complex build environments
- Multiple related submissions
- Existing open-source projects

**Best practices:**

1. **Use a dedicated repository** for CAPE submissions (avoid coupling to main project)
2. **Pin to specific versions** using git tags or commit hashes
3. **Provide direct clickable links** to exact source files
4. **Include build instructions** in the repository
5. **Keep repository public and stable** (avoid deletion or making private)

**Example `source/README.md` with clickable links:**

````markdown
# MyCompiler Fibonacci Implementation

**Source Code**: [Fibonacci.hs](https://github.com/username/cape-submissions/blob/v1.0.0/fibonacci/Fibonacci.hs)

**Repository**: https://github.com/username/cape-submissions **Tag**: `v1.0.0` (commit: `abc123def456...`) **Path**: `fibonacci/Fibonacci.hs`

This submission uses MyCompiler 1.0.0 with optimizations enabled.

## Reproducing the Compilation

1. Clone the repository:
   ```bash
   git clone https://github.com/username/cape-submissions
   cd cape-submissions
   ```
````

2. Check out the specific version:

   ```bash
   git checkout v1.0.0
   ```

3. Follow build instructions in the repository README

4. The compiled UPLC output should match `fibonacci.uplc` in this submission

For detailed build instructions and environment setup, see the repository README.

````

**GitHub URL formats for version-pinned links:**

- Single file: `https://github.com/{owner}/{repo}/blob/{tag-or-commit}/{path/to/file}`
- Directory: `https://github.com/{owner}/{repo}/tree/{tag-or-commit}/{path/to/directory}`

### Metadata Fields for Source Code

Your `metadata.json` should include these fields in the `submission` object:

- **`source_available`** (boolean, required): Whether source code is publicly available
- **`source_repository`** (string, optional): URL to source code repository
- **`source_commit_hash`** (string, optional): Full 40-character git commit hash

**Example:**

```json
{
  "submission": {
    "date": "2025-01-15T00:00:00Z",
    "source_available": true,
    "source_repository": "https://github.com/username/cape-submissions",
    "source_commit_hash": "abc123def456789abc123def456789abc123def4",
    "implementation_notes": "Iterative implementation with tail-call optimization"
  }
}
````

### Anti-patterns to Avoid

- ❌ **Referencing `main` branch without version**: Repository can evolve, breaking reproducibility
- ❌ **Referencing repositories that may disappear**: Use stable, dedicated repositories
- ❌ **Including source without build instructions**: Viewers won't be able to reproduce compilation
- ❌ **Mixing submission source with unrelated code**: Use dedicated repository or clear path structure
- ❌ **Omitting version information**: Always include tags or commit hashes for external repositories

### Quick Reference: When to Use Each Approach

| Factor | Inline Source | External Repository |
| --- | --- | --- |
| **Complexity** | Simple, single-file | Multi-file, complex builds |
| **Build Setup** | Minimal dependencies | Complex environment |
| **Multiple Submissions** | One-off submission | Series of related submissions |
| **Repository Size** | Small source files | Large codebase |
| **Maintenance** | Self-contained | Centralized updates |
| **Recommendation** | Good for simple cases | **Preferred for most cases** |

---

## File Structure

### Metadata Template

Use `submissions/TEMPLATE/metadata-template.json` as a starting point:

```json
{
  "compiler": {
    "name": "MyCompiler",
    "version": "1.0.0",
    "commit_hash": "abc123..." // Optional: 40-char git hash
  },
  "compilation_config": {
    "target": "uplc",
    "optimization_level": "O2",
    "flags": ["--optimize", "--inline"],
    "environment": {
      "dependencies": {
        "some-package": "1.2.3"
      }
    }
  },
  "contributor": {
    "name": "myhandle", // All contributor fields optional
    "organization": "My Company",
    "contact": "me@example.com"
  },
  "submission": {
    "date": "2025-01-15T00:00:00Z",
    "source_available": true,
    "source_repository": "https://github.com/me/my-repo",
    "implementation_notes": "Brief description of approach, algorithms used, and any notable optimizations."
  }
}
```

### Metrics Template

Generated automatically by `cape submission measure`, but follows this structure:

```json
{
  "scenario": "fibonacci",
  "version": "1.0.0",
  "measurements": {
    "cpu_units": {
      "maximum": 185916,
      "sum": 185916,
      "minimum": 185916,
      "median": 185916,
      "sum_positive": 185916,
      "sum_negative": 0
    },
    "memory_units": {
      "maximum": 592,
      "sum": 592,
      "minimum": 592,
      "median": 592,
      "sum_positive": 592,
      "sum_negative": 0
    },
    "script_size_bytes": 1234,
    "term_size": 45
  },
  "evaluations": [
    {
      "name": "test_case_name",
      "description": "Test case description",
      "cpu_units": 185916,
      "memory_units": 592,
      "execution_result": "success"
    }
  ],
  "execution_environment": {
    "evaluator": "plutus-core-executable-1.52.0.0"
  },
  "timestamp": "2025-01-15T00:00:00Z",
  "notes": "Optional notes about measurements."
}
```

---

## Validation Workflow

### 1. Create Submission Structure

```bash
cape submission new {scenario} {Compiler} {Version} {Handle}
# Example: cape submission new fibonacci MyCompiler 1.0.0 myhandle
```

This creates the directory structure and template files.

### 2. Add Your UPLC Program

Replace the placeholder UPLC file with your compiled program:

- Path: `submissions/{scenario}/{Compiler}_{Version}_{Handle}/{scenario}.uplc`
- Ensure it's a valid UPLC program that can be evaluated
- For fully-applied scenarios: Program should execute immediately
- For parameterized scenarios: Program should accept inputs as defined in scenario's `cape-tests.json`

### 3. Measure Performance

Use the unified measurement command:

```bash
# Measure from submission directory
cd submissions/{scenario}/{Compiler}_{Version}_{Handle}
cape submission measure .

# Or measure from anywhere
cape submission measure submissions/{scenario}/{Compiler}_{Version}_{Handle}

# Or measure all submissions
cape submission measure --all
```

This automatically:

- Evaluates your UPLC program against test cases
- Measures CPU units, memory units, script size, term size
- Generates or updates `metrics.json`
- Validates measurements against schema

### 4. Verify Correctness and Schema Compliance

```bash
# Verify specific submission
cape submission verify submissions/{scenario}/{Compiler}_{Version}_{Handle}

# Or verify all submissions
cape submission verify --all
```

This checks:

- UPLC program correctness (evaluates to expected result or passes all test cases)
- `metadata.json` validates against schema
- `metrics.json` validates against schema
- All required files are present

### 5. Provide Metadata

Edit `metadata.json` to include:

- Compiler information (name, version, optional commit hash)
- Compilation configuration (optimization level, flags, dependencies)
- Contributor information (optional, for privacy)
- Implementation notes (approach, algorithms, optimizations)

Validate with:

```bash
cape submission verify submissions/{scenario}/{Compiler}_{Version}_{Handle}
```

### 6. Document Your Approach

Edit `README.md` in your submission directory to explain:

- Implementation approach
- Algorithmic choices
- Compiler-specific optimizations used
- Known limitations or caveats
- For prescribed algorithm scenarios: How your implementation matches the required algorithm

---

## Common Checklist

Before submitting your implementation:

- [ ] **UPLC Program**: Valid program that evaluates correctly
- [ ] **Correctness**: Produces expected output for all test cases
- [ ] **Budget Compliance**: Executes within CEK machine limits
- [ ] **Determinism**: Consistent results across multiple runs
- [ ] **Metadata**: `metadata.json` validates against schema
- [ ] **Metrics**: `metrics.json` generated and validates against schema
- [ ] **README**: Implementation approach documented
- [ ] **Source Code** (if available): Included in `source/` directory
- [ ] **Verification**: `cape submission verify` passes
- [ ] **Algorithm Compliance** (for prescribed scenarios): Implementation matches required algorithm

### Testing Commands

```bash
# Measure performance
cape submission measure .

# Verify correctness and schemas
cape submission verify .

# Check budget compliance
# (automatically checked during measure and verify)

# Test determinism
cape submission measure . && cape submission measure .
# (metrics should be identical)
```

---

## Additional Resources

- **Scenario Specifications**: See `scenarios/{scenario}/{scenario}.md` for scenario-specific requirements
- **Schema Files**: `submissions/TEMPLATE/*.schema.json` for validation requirements
- **Template Files**: `submissions/TEMPLATE/*-template.json` for starting points
- **CLI Reference**: See [USAGE.md](../USAGE.md) for complete command documentation
- **Project README**: See [README.md](../README.md) for project overview

---

## Getting Help

If you encounter issues or have questions:

1. Check the scenario-specific documentation in `scenarios/{scenario}/`
2. Review existing submissions for examples
3. Consult the CLI help: `cape submission --help`
4. Open an issue on GitHub: https://github.com/IntersectMBO/UPLC-CAPE/issues

---

_Happy benchmarking! We look forward to your submissions and insights into compiler performance._
