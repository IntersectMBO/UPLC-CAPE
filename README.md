# UPLC-CAPE

**Comparative Artifact Performance Evaluation for UPLC**

A standardized framework for benchmarking the on-chain performance of programs produced by various compilers in the Cardano ecosystem.

## Overview

UPLC-CAPE provides a community-driven benchmarking suite that enables compiler authors to measure and compare the efficiency of their generated UPLC (Untyped Plutus Core) code. This framework focuses on benchmarking the performance of compiled artifacts rather than the compilation process itself.

## Development Environment

This project uses Nix for reproducible development environments. To get started:

```bash
# Enter the development shell
nix develop

# Or if using direnv (recommended)
direnv allow
```

## Architecture Decision Records (ADRs)

This project uses Architecture Decision Records to document important architectural and design decisions. ADRs are managed using [Log4brains](https://github.com/thomvaill/log4brains).

### ADR Commands

The development environment includes convenient short commands for managing ADRs:

```bash
# Create a new ADR
adr new "My Decision Title"

# Preview ADRs in your browser
adr preview

# Build static documentation site
adr build

# Show help and available commands
adr help
```

### Single Letter Aliases

For even faster usage, single letter aliases are available:

```bash
adr n "Quick ADR"    # new
adr p               # preview  
adr b               # build
adr h               # help
```

### ADR Workflow

1. **Before making significant decisions**: Check existing ADRs in `doc/adr/`
2. **When making a new decision**: Create an ADR using `adr new "Decision Title"`
3. **Follow the template**: Fill out Context, Decision, and Consequences sections
4. **Review process**: Use `adr preview` to review your ADR in the browser
5. **Commit**: Include the ADR in your pull request with the related changes

### Viewing ADRs

- **Browse locally**: Run `adr preview` to start a local web server
- **Read directly**: ADR files are in `doc/adr/` as Markdown files
- **Build static site**: Use `adr build` to generate a deployable documentation site

### Current ADRs

- [ADR 0001: Record architecture decisions](doc/adr/0001-record-architecture-decisions.md)
- [ADR 0002: Use Log4brains for ADR management](doc/adr/0002-use-log4brains-for-adr-management.md)

## Contributing

When contributing to this project:

1. Enter the development environment: `nix develop`
2. Check existing ADRs to understand current architectural decisions
3. Create an ADR for any significant architectural changes: `adr new "Your Decision"`
4. Follow the established patterns and guidelines documented in the ADRs
5. Refer to [CONTRIBUTING.md](CONTRIBUTING.md) for detailed submission guidelines and templates

## Resources

- [Project Epic](.md/EPIC.md) - Detailed project description and goals
- [Log4brains Documentation](https://github.com/thomvaill/log4brains) - ADR tool documentation
- [Architecture Decision Records](http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions) - ADR concept by Michael Nygard
