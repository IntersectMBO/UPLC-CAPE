# UPLC-CAPE Architecture Knowledge Base

**Comparative Artifact Performance Evaluation for UPLC**

Welcome to the architectural documentation for UPLC-CAPE, a standardized
framework for benchmarking the on-chain performance of programs produced by
various compilers in the Cardano smart contract ecosystem.

## About This Project

UPLC-CAPE addresses a critical need in the Cardano ecosystem: the lack of
standardized performance comparison tools for compiler-generated UPLC (Untyped
Plutus Core) code. This framework enables compiler authors to measure and
compare the efficiency of their generated artifacts through standardized
benchmark scenarios.

### Key Goals

- **Standardized Benchmarking**: Provide consistent metrics including CPU units,
  memory units, script size, and term size
- **Community Resource**: Independent framework serving all compiler teams
  (Plinth, Aiken, Plutarch, etc.)
- **Reproducible Results**: Comprehensive metadata ensures deterministic and
  reproducible benchmarks
- **Performance Insights**: Help identify optimization opportunities and
  quantify performance improvements

### Framework Focus

This framework benchmarks the **performance of compiled artifacts** (script
efficiency, execution costs) rather than the compilation process itself
(compilation speed, compiler memory usage).

## Benchmark Scenarios

The framework includes both synthetic and real-world smart contract scenarios:

- **Synthetic Benchmarks**: Fibonacci sequences, recursive algorithms
- **Real-World Contracts**: Two-party escrow, streaming payments, DAO voting,
  time-locked staking

Each scenario provides standardized specifications that compiler teams can
implement in their respective languages and frameworks.

## Architecture Decision Records

This knowledge base documents the architectural and design decisions that shape
UPLC-CAPE. These ADRs provide context for contributors and maintainers about why
certain approaches were chosen.

### ADR Usage Cheatsheet

Quick reference for managing ADRs in this project:

```bash
# Create a new architectural decision record
adr new "Your Decision Title"

# Preview all ADRs in your browser (recommended for review)
adr preview

# Build static documentation site for deployment
adr build

# Show all available commands and options
adr help
```

**Single letter shortcuts:** `adr n` (new), `adr p` (preview), `adr b` (build),
`adr h` (help)

### ADR Workflow

1. **Research**: Review existing ADRs to understand current architecture
2. **Create**: Use `adr new "Title"` when making significant decisions
3. **Document**: Fill out Context, Decision, and Consequences sections
4. **Review**: Use `adr preview` to review your ADR
5. **Commit**: Include ADR with related implementation changes

## Getting Started

To contribute to this project:

1. Set up the development environment with `nix develop`
2. Review existing ADRs to understand architectural context
3. Follow the established patterns documented in the ADRs
4. Create ADRs for significant architectural changes

---

_This knowledge base is powered by
[Log4brains](https://github.com/thomvaill/log4brains) and managed through
Architecture Decision Records._
