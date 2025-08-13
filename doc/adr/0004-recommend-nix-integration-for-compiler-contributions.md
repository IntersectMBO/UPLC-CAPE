# Recommend Nix Integration for Compiler Contributions

- Status: accepted
- Date: 2025-07-18
- Tags: contribution-workflow, nix, development-environment

## Context and Problem Statement

Contributors wanting to submit benchmark implementations need a consistent and reproducible development environment to write, compile, and evaluate their implementations. Currently, contributors must set up their own development environments, which can lead to inconsistencies in compilation results and difficulty reproducing benchmark outcomes across different systems.

How can we provide a standardized development environment that supports multiple compilers while maintaining flexibility for contributors who prefer their own tooling?

## Decision Drivers

- Need for reproducible compilation environments across different systems
- Desire to lower the barrier to entry for new contributors
- Requirement to support multiple compiler toolchains (Aiken, Plutarch, Helios, etc.)
- Existing project infrastructure already uses Nix for development environment
- Need to maintain flexibility for contributors who prefer external tooling
- Goal of ensuring consistent benchmark results regardless of contributor's local setup

## Considered Options

1. **Recommend Nix integration with optional external tooling**
2. **Require all contributions to use Nix environment**
3. **Provide no standardized environment recommendations**
4. **Use Docker containers for standardized environments**

## Decision Outcome

Chosen option: "Recommend Nix integration with optional external tooling", because it provides the best balance between standardization and flexibility. This approach leverages the existing Nix infrastructure while not forcing contributors into a specific workflow.

### Positive Consequences

- Contributors can extend the existing Nix environment with their compiler tools
- Reproducible compilation environments across different systems
- Lower barrier to entry for new contributors who can use the provided environment
- Consistent evaluation and testing environment for benchmark implementations
- Maintains flexibility for experienced contributors who prefer their own tooling
- Leverages existing project infrastructure and tooling

### Negative Consequences

- Contributors need to learn Nix if they want to use the recommended approach
- Maintaining compiler dependencies in the Nix flake requires ongoing effort
- Some contributors may still choose external tooling, leading to potential inconsistencies
- Additional complexity in the contribution documentation

## Pros and Cons of the Options

### Recommend Nix integration with optional external tooling

- **Good**: Provides standardization while maintaining flexibility
- **Good**: Leverages existing project infrastructure
- **Good**: Reproducible environments for consistent results
- **Good**: Lowers barrier to entry for new contributors
- **Bad**: Requires Nix knowledge for optimal contribution experience
- **Bad**: Ongoing maintenance of compiler dependencies

### Require all contributions to use Nix environment

- **Good**: Maximum standardization and reproducibility
- **Good**: Consistent compilation environments
- **Bad**: High barrier to entry for contributors unfamiliar with Nix
- **Bad**: May discourage contributions from experienced developers with existing workflows
- **Bad**: Inflexible approach that may not suit all compiler ecosystems

### Provide no standardized environment recommendations

- **Good**: Maximum flexibility for contributors
- **Good**: No additional infrastructure to maintain
- **Bad**: Inconsistent compilation environments
- **Bad**: Difficult to reproduce benchmark results
- **Bad**: Higher barrier to entry for new contributors
- **Bad**: Potential for environment-specific compilation issues

### Use Docker containers for standardized environments

- **Good**: Standardized environments without Nix complexity
- **Good**: More familiar to many developers than Nix
- **Bad**: Requires maintaining separate infrastructure from existing Nix setup
- **Bad**: Less efficient than Nix for development workflows
- **Bad**: Additional complexity in maintaining Docker images
- **Bad**: Doesn't leverage existing project tooling

## Implementation Notes

The recommended workflow includes:

1. Fork the repository
2. Add required compiler pipeline and tools to the Nix shell via `flake.nix`
3. Use the available compiler tools within the Nix development shell
4. Write, compile, and evaluate benchmark implementations in the standardized environment
5. Prepare submissions following the established structure and file requirements

This approach is documented in `CONTRIBUTING.md` as the recommended but not mandatory workflow, with the standard external tooling approach remaining available as an alternative.
