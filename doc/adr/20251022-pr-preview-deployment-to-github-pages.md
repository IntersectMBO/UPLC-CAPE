# PR Preview Deployment to GitHub Pages

- Status: accepted
- Date: 2025-10-22
- Tags: ci-cd, github-actions, deployment, developer-experience

## Context and Problem Statement

When reviewers evaluate pull requests that add or modify benchmark submissions, they need to see how the changes affect the generated HTML reports. Currently, reviewers must:

1. Pull the PR branch locally
2. Set up the Nix environment
3. Run `cape submission measure --all`
4. Run `cape submission report --all`
5. Open the generated reports in a browser

This creates friction in the review process and makes it harder to spot performance regressions or improvements.

**Question**: How can we provide automatic, isolated preview sites for PRs that modify submission data?

## Decision Drivers

- **Visual feedback**: Reviewers need to see report changes without local setup
- **Isolation**: Multiple PRs should have separate previews without conflicts
- **Automation**: Previews should update automatically on push
- **Cleanup**: Remove previews when PRs close to avoid clutter
- **Efficiency**: Only generate reports when submission data actually changes
- **Compatibility**: Coexist with existing `static.yml` workflow for main branch

## Considered Options

### 1. Official GitHub Pages Actions (actions/deploy-pages)

**Approach**: Use the official `actions/upload-pages-artifact` and `actions/deploy-pages` actions.

**Pros**:

- Official GitHub solution
- Well-maintained and documented
- Already used in `static.yml`

**Cons**:

- Deploys to root of GitHub Pages site (replaces entire site)
- Cannot deploy to subdirectories
- Would conflict with main branch deployments
- Each deployment overwrites previous content

**Verdict**: ❌ Not suitable for PR previews due to lack of subdirectory support

### 2. Git-based Deployment with peaceiris/actions-gh-pages

**Approach**: Push report content directly to the `gh-pages` branch under PR-specific subdirectories.

**Pros**:

- Supports `destination_dir` for subdirectory deployments
- `keep_files: true` preserves other PR previews
- Compatible with existing main branch deployment
- Widely used and battle-tested action
- Automatic conflict resolution

**Cons**:

- Slightly more complex than official actions
- Direct git operations on gh-pages branch

**Verdict**: ✅ Chosen solution

### 3. Netlify/Vercel Preview Deployments

**Approach**: Use external preview deployment services.

**Pros**:

- Purpose-built for preview deployments
- Advanced features (custom domains, redirects)

**Cons**:

- Requires third-party service setup
- Additional authentication/secrets management
- Project already uses GitHub Pages
- Adds external dependency

**Verdict**: ❌ Unnecessary complexity

## Decision Outcome

Chosen option: **"Git-based Deployment with peaceiris/actions-gh-pages"**, because it:

- Enables subdirectory deployments (`pr-<number>/`)
- Preserves existing previews and main branch site
- Integrates seamlessly with existing GitHub Pages setup
- Provides automatic cleanup workflow
- Requires no additional external services

### Implementation Overview

**Two GitHub Actions workflows:**

#### 1. PR Preview Workflow (`.github/workflows/pr-preview.yml`)

**Triggers**:

- Pull request opened, synchronized, or reopened
- **Path filter**: Only runs when `submissions/**` changes

**Optimization Strategy (Hybrid Approach)**:

1. **Coarse-grained filter**: `paths: submissions/**` prevents workflow execution for non-submission PRs
2. **Fine-grained validation**: Git diff checks if `.uplc` or `metadata.json` files changed
3. **Early exit**: Skips report generation if only documentation/README changed

**Key Steps**:

1. Checkout PR branch with full history (`fetch-depth: 0`)
2. Check if submission data files actually changed:
   ```bash
   git diff --name-only origin/main...HEAD | grep -qE 'submissions/.*\.(uplc|metadata\.json)$'
   ```
3. If no data changes: Skip remaining steps (optimization)
4. If data changed:
   - Set up Nix environment with Cachix
   - Build `measure` executable
   - Run `cape submission measure --all`
   - Run `cape submission report --all`
   - Deploy to `gh-pages` under `pr-<number>/` subdirectory
   - Post sticky comment with preview URL

**Deployment Configuration**:

```yaml
uses: peaceiris/actions-gh-pages@v4
with:
  github_token: ${{ secrets.GITHUB_TOKEN }}
  publish_dir: ./report
  destination_dir: pr-${{ github.event.pull_request.number }}
  keep_files: true
```

**Comment Integration**:

```yaml
uses: marocchino/sticky-pull-request-comment@v2
with:
  header: pr-preview
  message: |
    ## 🚀 PR Preview Deployed
    Preview URL: https://intersectmbo.github.io/UPLC-CAPE/pr-<number>/
```

#### 2. Cleanup Workflow (`.github/workflows/cleanup-preview.yml`)

**Triggers**:

- Pull request closed (merged or abandoned)

**Key Steps**:

1. Checkout `gh-pages` branch
2. Remove `pr-<number>/` directory if exists
3. Commit and push changes

**Idempotency**: Safely handles cases where preview directory doesn't exist.

### Concurrency Control

```yaml
concurrency:
  group: pr-preview-${{ github.event.pull_request.number }}
  cancel-in-progress: true
```

- Prevents multiple simultaneous deployments for the same PR
- Cancels in-progress runs when new commits are pushed

### Preview URL Pattern

```
https://intersectmbo.github.io/UPLC-CAPE/pr-<number>/
```

**Examples**:

- PR #42: `https://intersectmbo.github.io/UPLC-CAPE/pr-42/`
- PR #110: `https://intersectmbo.github.io/UPLC-CAPE/pr-110/`

### Coexistence with Main Branch Deployment

- **Main branch** (`static.yml`): Deploys to root using `actions/deploy-pages`
- **PR branches** (`pr-preview.yml`): Deploy to subdirectories using `peaceiris/actions-gh-pages`
- **No conflicts**: Different deployment mechanisms target different locations

## Positive Consequences

- **Improved review workflow**: Reviewers see visual changes immediately
- **No local setup required**: Preview sites accessible from any browser
- **Isolated environments**: Each PR has independent preview
- **Automatic updates**: Previews refresh on every push
- **Automatic cleanup**: No manual intervention needed when PRs close
- **Resource efficiency**: Only generates reports when submission data changes
- **Smart filtering**: Avoids unnecessary CI runs for documentation-only changes
- **Sticky comments**: Preview link stays at top of PR conversation
- **Zero configuration**: Uses default `GITHUB_TOKEN`, no secrets needed

## Negative Consequences

- **CI resource usage**: Measuring and reporting adds ~5-10 minutes per PR
  - Mitigated by: Path filters and git diff checks
- **gh-pages branch growth**: Each PR adds a subdirectory
  - Mitigated by: Automatic cleanup on PR close
- **Preview URL lifetime**: Links become invalid after PR closes
  - Acceptable: Previews are temporary by design
- **Concurrent PR limitations**: Many simultaneous PRs could create many subdirectories
  - Acceptable: Cleanup workflow keeps it manageable

## Hybrid Optimization Strategy

The implementation uses a two-layer filtering approach:

### Layer 1: GitHub Paths Filter (Workflow Execution Gate)

```yaml
on:
  pull_request:
    paths:
      - "submissions/**"
```

**Effect**: Prevents workflow from running for PRs that don't touch `submissions/` at all.

**Catches**:

- Documentation changes in root
- Code changes in `lib/`, `measure-app/`, etc.
- CI/CD configuration changes

### Layer 2: Git Diff Validation (Report Generation Gate)

```bash
git diff --name-only origin/main...HEAD | grep -qE 'submissions/.*\.(uplc|metadata\.json)$'
```

**Effect**: Skips report generation if only non-data files changed in `submissions/`.

**Catches**:

- README updates in submission directories
- Documentation improvements in `source/` directories
- Comment changes in metadata

### Combined Efficiency

**Scenario 1**: PR modifies `lib/Cape/Compile.hs`

- ✅ Layer 1 blocks → Workflow doesn't run at all

**Scenario 2**: PR updates `submissions/fibonacci/Plinth_1.0.0_baseline/README.md`

- ❌ Layer 1 allows → Workflow starts
- ✅ Layer 2 blocks → Early exit, no report generation

**Scenario 3**: PR adds new `submissions/fibonacci/MyCompiler_2.0.0_handle/fibonacci.uplc`

- ❌ Layer 1 allows → Workflow starts
- ❌ Layer 2 allows → Full report generation and deployment

## Alternative Approaches Considered

### Single-Layer Filtering

**Option A**: Only use paths filter

- Simpler configuration
- Wastes CI resources on README-only changes

**Option B**: Only use git diff check

- Workflow runs for all PRs
- Adds unnecessary setup overhead

**Decision**: Hybrid approach provides best balance of simplicity and efficiency.

## Documentation Updates

### README.md

Expanded **"Live Performance Reports"** section to include:

- New subsection on PR Preview Sites
- Preview URL pattern explanation
- Trigger conditions (submission data changes)
- Automatic update and cleanup behavior
- Reference to this ADR

### USAGE.md

Added brief note about:

- PR preview functionality
- When previews are generated
- Reference to README.md for details

## Testing Considerations

**Manual Testing**:

1. Create PR with submission changes → Verify preview deploys
2. Push additional commits → Verify preview updates
3. Create PR with only README changes → Verify no deployment
4. Close PR → Verify cleanup removes directory

**Validation Points**:

- Preview URL returns 200 status
- Report content reflects PR branch data
- Sticky comment appears on PR
- Preview directory removed after close

## Links

- Related Issue: #110
- Deployment Action: [peaceiris/actions-gh-pages](https://github.com/peaceiris/actions-gh-pages)
- Comment Action: [marocchino/sticky-pull-request-comment](https://github.com/marocchino/sticky-pull-request-comment)
- Related Workflow: `.github/workflows/static.yml` (main branch deployment)
- GitHub Pages: https://intersectmbo.github.io/UPLC-CAPE/
