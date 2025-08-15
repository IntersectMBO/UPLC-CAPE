# Plutus 1.52.0.0 Upgrade Status

## ✅ Completed Tasks

### Configuration Updates
- **plinth/uplc-cape-benchmarks.cabal**: Updated all Plutus dependencies from `>=1.49` to `>=1.52`
- **measure/uplc-measure.cabal**: Updated all Plutus dependencies from `>=1.49` to `>=1.52`
- **README.md**: Updated package baselines from 1.49.0.0 to 1.52.0.0
- **flake.nix**: Added TODO comment with instructions for pinning Plutus 1.52.0.0

### Documentation Updates  
- **README.md**: Updated all version examples from 1.49.0.0 to 1.52.0.0
- **doc/domain-model.md**: Updated version examples in compiler documentation
- **.github/copilot-instructions.md**: Created new file with version requirements
- **submissions/TEMPLATE/metadata.schema.json**: Updated version examples
- **submissions/TEMPLATE/metrics.schema.json**: Updated evaluator examples
- **scripts/cape-subcommands/submission/new.help.tmpl**: Updated help examples
- **scripts/cape-subcommands/submission/measure.help.tmpl**: Updated path examples
- **scripts/cape-subcommands/submission/new.sh**: Updated version prompts

### Code Quality
- Applied prettier formatting to all markdown and JSON files
- Ensured consistent formatting across the repository

## 🔄 Remaining Manual Tasks

The following tasks require a Nix environment with network access and cannot be completed without it:

### 1. Update Nix Flake for Plutus 1.52.0.0

```bash
# Find the exact commit for Plutus 1.52.0.0
# Visit: https://github.com/IntersectMBO/plutus/releases/tag/1.52.0.0
# Get the commit hash and update flake.nix:

# Replace this line in flake.nix:
# url = "github:IntersectMBO/plutus";
# With:
# url = "github:IntersectMBO/plutus/COMMIT_HASH_FOR_1.52.0.0";

# Then update the lockfile:
nix flake update
```

### 2. Test the Build Environment

```bash
# Enter Nix development shell
nix develop

# Test that everything builds correctly
cabal build all --project-file=plinth/cabal.project
cabal build all --project-file=measure/cabal.project

# Fix any import/API changes if the build fails
# (Plutus 1.52.0.0 may have breaking changes from 1.49.x)
```

### 3. Regenerate All Measurements

```bash
# In Nix shell, regenerate all benchmark metrics
cape submission measure --all

# This will update all metrics.json files with new evaluator versions
# and potentially different performance measurements
```

### 4. Regenerate Reports

```bash
# Rebuild and regenerate HTML reports with updated metrics
cape submission report --all

# This will update report/benchmarks/*.html files
# with new measurement data and evaluator information
```

### 5. Validate and Test

```bash
# Validate all schemas
cape submission validate --all

# Run the comprehensive test suite
bash scripts/cape-subcommands/test/test.sh

# Format all files
treefmt

# Check that CI passes
git add .
git commit -m "Regenerate measurements and reports for Plutus 1.52.0.0"
git push
```

## 📋 Expected Changes After Manual Steps

### Files that will be updated:
- `flake.lock` - New Plutus commit hash and dependencies
- `submissions/*/metrics.json` - Updated evaluator versions and potentially different measurements
- `report/benchmarks/*.html` - Updated reports with new measurement data
- Any Haskell source files if API changes require import updates

### Potential Issues:
- **Breaking API changes**: Plutus 1.52.0.0 may have breaking changes requiring code updates
- **Performance changes**: CEK costing may have changed, resulting in different CPU/memory measurements
- **Build dependencies**: CHaP index may need updating for Plutus 1.52.0.0 compatibility

## 🎯 Final Validation

After completing all manual steps:

1. Ensure all builds pass in Nix shell
2. Verify CI is green 
3. Check that generated reports render correctly
4. Confirm all measurement files validate against schemas
5. Test that the CLI commands work as expected

## 📚 Reference Documentation

- [Issue #14](https://github.com/IntersectMBO/UPLC-CAPE/issues/14) - Original upgrade request
- [Plutus 1.52.0.0 Release](https://github.com/IntersectMBO/plutus/releases/tag/1.52.0.0) - Release notes
- [CAPE Contributing Guide](CONTRIBUTING.md) - Development workflow
- [Nix Development Guide](README.md#quick-start) - Setting up the development environment