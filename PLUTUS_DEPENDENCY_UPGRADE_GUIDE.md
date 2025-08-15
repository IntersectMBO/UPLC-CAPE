# Plutus Dependency Upgrade Guide

A comprehensive guide for upgrading Plutus dependencies in UPLC-CAPE, based on lessons learned from the 1.52.0.0 upgrade.

## Overview

Upgrading Plutus dependencies in UPLC-CAPE involves multiple components due to the Nix-based development environment and CHaP (Cardano Haskell Packages) ecosystem. This guide provides a systematic approach to handle common challenges.

## Pre-Upgrade Checklist

### 1. Version Verification

```bash
# Check if the target Plutus version actually exists
# Visit: https://github.com/IntersectMBO/plutus/releases
# Verify the exact commit hash for the version tag
```

**⚠️ Critical**: Ensure the target version is actually released. Version tags may not exist even if referenced in documentation.

### 2. CHaP Package Availability

```bash
# Check if packages are available in CHaP
# Visit: https://chap.intersectmbo.org/
# Or check the CHaP repository for recent commits that might include your version
```

**Key Insight**: Plutus packages may not be immediately available in CHaP even after source release.

## Step-by-Step Upgrade Process

### Phase 1: Update Configuration Files

#### 1.1 Update Cabal Dependency Constraints

```bash
# Update in both files:
# - plinth/uplc-cape-benchmarks.cabal
# - measure/uplc-measure.cabal

# Change from:
plutus-core        >=1.XX
plutus-ledger-api  >=1.XX
plutus-tx          >=1.XX
plutus-tx-plugin   >=1.XX

# To:
plutus-core        >=1.YY
plutus-ledger-api  >=1.YY
plutus-tx          >=1.YY
plutus-tx-plugin   >=1.YY
```

#### 1.2 Update Documentation

Update version references in:

- `README.md` (package baselines section)
- `submissions/TEMPLATE/*.schema.json` (examples)
- Help templates in `scripts/cape-subcommands/*/`

### Phase 2: Handle Nix and CHaP Issues

#### 2.1 Identify the Problem

```bash
nix develop
# If you get dependency resolution errors, the packages aren't in CHaP yet
```

#### 2.2 Solution: Use Source Repository Packages

When CHaP packages aren't available, add to both `cabal.project` files:

```cabal
source-repository-package
  type: git
  location: https://github.com/IntersectMBO/plutus
  tag: COMMIT_HASH_FOR_TARGET_VERSION
  --sha256: SHA256_HASH_FROM_NIX_LOGS
  subdir:
    plutus-core
    plutus-ledger-api
    plutus-tx
    plutus-tx-plugin
    plutus-metatheory
```

**⚠️ Important**: Add the `--sha256` line to eliminate nix warnings. The hash will be provided in the nix build logs if missing.

**🔍 Finding Subdirectories**: Check the target commit to see which subdirectories exist. Common ones:

- `plutus-core` (always)
- `plutus-ledger-api` (always)
- `plutus-tx` (always)
- `plutus-tx-plugin` (always)
- `plutus-metatheory` (if referenced as dependency)
- `prettyprinter-configurable` (may not exist in newer versions)

#### 2.3 Handle Dependency Conflicts

Common issue: Version conflicts with transitive dependencies like QuickCheck.

Add to both `cabal.project` files:

```cabal
allow-newer:
  -- Resolve version conflicts with new Plutus version
  *:QuickCheck
  , ral:QuickCheck
  , fin:QuickCheck
```

**Pattern**: Use `*:PACKAGE` for broad compatibility or `specific-package:PACKAGE` for targeted fixes.

#### 2.4 Update Index States

```cabal
index-state:
  -- Use dates at or before the latest available indices
  , hackage.haskell.org YYYY-MM-DDTHH:MM:SSZ
  , cardano-haskell-packages YYYY-MM-DDTHH:MM:SSZ
```

**💡 Tip**: If you get "index-state is newer than available" errors, check the error message for the actual latest available date.

### Phase 3: Update Flake Dependencies

#### 3.1 Update flake.nix

```nix
plutus = {
  url = "github:IntersectMBO/plutus/EXACT_COMMIT_HASH";
  inputs.hackage.follows = "hackage";
  inputs.CHaP.follows = "CHaP";
  inputs.haskell-nix.follows = "haskell-nix";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

#### 3.2 Update flake.lock

```bash
nix flake update
```

### Phase 4: Build and Test

#### 4.1 Test Build Environment

```bash
nix develop --command bash -c "cabal update && cabal build all --project-file=plinth/cabal.project"
nix develop --command bash -c "cabal build all --project-file=measure/cabal.project"
```

#### 4.2 Regenerate Measurements

```bash
nix develop --command cape submission measure --all
```

#### 4.3 Update Reports

```bash
nix develop --command cape submission report --all
```

#### 4.4 Validate Everything

```bash
nix develop --command cape submission validate --all
nix develop --command cape test
nix develop --command treefmt
```

## Common Issues and Solutions

### Issue 1: "Could not resolve dependencies"

**Symptom**: Cabal cannot find required Plutus version **Solution**: Use `source-repository-package` as shown in Phase 2.2

### Issue 2: "index-state is older than requested"

**Symptom**: Index state in cabal.project is too new **Solution**: Update index-state to available dates as shown in Phase 2.4

### Issue 3: QuickCheck version conflicts

**Symptom**: `conflict: package-a => QuickCheck>=X.Y but package-b => QuickCheck^>=W.Z` **Solution**: Add `allow-newer` entries as shown in Phase 2.3

### Issue 4: Missing subdirectories

**Symptom**: `does not exist (No such file or directory)` for subdirs **Solution**: Check the actual Plutus repository structure at the target commit

### Issue 5: Build succeeds but wrong evaluator version

**Symptom**: Measurements still show old evaluator version **Solution**: Ensure you're using the correct Plutus source and regenerate measurements

### Issue 6: Nix sha256 warnings

**Symptom**: `WARNING: No sha256 found for source-repository-package` **Solution**: Add `--sha256: HASH` line to source-repository-package entries using hash from nix logs

## Verification Checklist

After completing the upgrade:

- [ ] `nix develop` enters shell successfully without sha256 warnings
- [ ] Both plinth and measure projects build
- [ ] `cape submission measure --all` uses new evaluator version
- [ ] All tests pass: `cape test`
- [ ] All validations pass: `cape submission validate --all`
- [ ] Reports generate successfully: `cape submission report --all`
- [ ] Code is properly formatted: `treefmt`

## Advanced Tips

### For LLM Agents

1. **Always verify version existence** before starting the upgrade
2. **Check CHaP availability** - if packages aren't in CHaP, use source-repository-package
3. **Handle dependency conflicts systematically** using allow-newer
4. **Add sha256 hashes** to eliminate nix warnings (check nix build logs for the hash)
5. **Verify successful upgrade** by checking evaluator version in metrics.json files
6. **Update documentation comprehensively** - version numbers appear in many places

### For Human Developers

1. **Budget extra time** for dependency resolution issues
2. **Test incrementally** - build after each major configuration change
3. **Keep backups** of working configurations before starting
4. **Monitor CHaP updates** - packages may become available later, allowing cleanup of source-repository-package entries
5. **Document any workarounds** for future reference

## Future Improvements

Consider these enhancements for easier future upgrades:

1. **Automated version checking** scripts
2. **CHaP availability detection** before attempting upgrades
3. **Template configurations** for common source-repository-package setups
4. **Integration tests** that verify evaluator versions in CI
5. **Rollback procedures** for failed upgrades

---

_This guide is based on the successful upgrade from Plutus 1.49.0.0 to 1.52.0.0 in August 2025. Update this document as the Plutus ecosystem evolves._
