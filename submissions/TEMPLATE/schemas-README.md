# JSON Schema Definitions

This directory contains JSON Schema definitions for validating the data files used in the UPLC-CAPE framework. These schemas are part of the submission template files.

## Schema Files

### `metrics.schema.json`

Validates the structure of benchmark metrics files that contain performance measurements and execution environment information.

**Required fields:**

- `scenario`: The benchmark scenario name
- `version`: Schema version (semantic versioning)
- `measurements`: Performance measurements object containing:
  - `cpu_units`: CPU execution units consumed
  - `memory_units`: Memory units consumed
  - `script_size_bytes`: Size of serialized UPLC script in bytes
  - `term_size`: Number of AST nodes in the UPLC term
- `execution_environment`: Evaluation environment information containing:
  - `evaluator`: Name and version of the evaluator used
- `timestamp`: ISO-8601 timestamp when measurements were taken

**Optional fields:**

- `notes`: Additional notes about the implementation or measurements

### `metadata.schema.json`

Validates the structure of submission metadata files that contain information about the compiler, compilation configuration, and contributor details.

**Required fields:**

- `compiler`: Compiler information containing:
  - `name`: Name of the compiler or toolchain
  - `version`: Version of the compiler
  - `commit_hash`: Git commit hash (optional)
- `compilation_config`: Compilation configuration containing:
  - `target`: Must be "uplc"
  - `optimization_level`: Optimization level used (optional)
  - `flags`: Additional compiler flags (optional)
  - `environment`: Environment configuration (optional)
- `submission`: Submission information containing:
  - `date`: ISO-8601 timestamp when submission was created
  - `source_available`: Whether source code is publicly available
  - `implementation_notes`: Notes about the implementation

**Optional fields:**

- `contributor`: Contributor information (all subfields optional for privacy)
- `source_repository`: URL to source repository (if source_available is true)

## Usage

### With CAPE Command Line Tools (Recommended)

The easiest way to verify your submissions is using the unified CAPE command:

```bash
# Verify all submissions in the project (correctness + schema validation)
cape submission verify --all

# Verify a specific submission directory
cape submission verify submissions/fibonacci/Aiken_1.0.8_myhandle/
```

### With check-jsonschema (Manual)

If you prefer to use check-jsonschema directly, these schemas can be used with any JSON Schema validation tool:

```bash
# Using check-jsonschema (available in CAPE nix shell)
# Validate a metrics file
check-jsonschema --schemafile submissions/TEMPLATE/metrics.schema.json path/to/metrics.json

# Validate a metadata file
check-jsonschema --schemafile submissions/TEMPLATE/metadata.schema.json path/to/metadata.json
```

## Schema Version

Both schemas use JSON Schema Draft 7 for maximum compatibility with validation tools and editors.
