# Contributing to UPLC-CAPE

Thank you for your interest in contributing benchmarks to UPLC-CAPE.  
This guide explains how to submit your compiled UPLC program and associated metrics.

## Directory Structure for Submissions

Each scenario has its own folder under `benchmarks/`.  
Inside `benchmarks/<scenario>/`, create a unique submission folder (e.g. a timestamp or your initials).  
The submission folder **must** contain:

1. **`<scenario>.uplc`**  
   - The fully-applied UPLC program with the benchmark target baked in (e.g. `fibonacci.uplc`).

2. **`metrics.json`**  
   - Performance measurements following the schema in `templates/metrics-template.json`.

3. **`metadata.json`**  
   - Compiler metadata following the schema in `templates/metadata-template.json`.  
   - For baseline scenarios, use `"compiler": { "name": "plinth", ... }`.

4. *(Optional)* **`source/`**  
   - Original source code (e.g. `fibonacci.plinth`) if you choose to publish it.

5. *(Optional)* **`config.json`**  
   - Compilation parameters (flags, optimization levels) that affect the generated UPLC.

6. **`README.md`**  
   - Implementation notes using the template from `templates/benchmark-README-template.md`.

## Submission Process

1. Fork this repository and clone your fork.
2. Copy or symlink your UPLC program and metrics into the appropriate folder:
   ```bash
   cd benchmarks/<scenario>
   mkdir <submission-id>
   cp path/to/your-program.uplc <submission-id>/<scenario>.uplc
   cp metrics.json metadata.json <submission-id>/
   ```
3. (Optional) Add `source/` and `config.json` if you wish to share your source or compilation parameters.
4. Add a `README.md` in your submission folder.  
   Use the template: `cp templates/benchmark-README-template.md benchmarks/<scenario>/<submission-id>/README.md`.
5. Commit and push to your fork.
6. Open a pull request against the `main` branch.

## Templates and Examples

- **Metrics Template**: `templates/metrics-template.json`  
- **Metadata Template**: `templates/metadata-template.json`  
- **Submission Guide**: `templates/submission-guide.md`  
- **Benchmark README**: `templates/benchmark-README-template.md`

## Future Automation

Eventually, automated tooling in `tools/` will help validate and measure UPLC programs.  
For now, please follow this manual process and template schemas to ensure consistency.
