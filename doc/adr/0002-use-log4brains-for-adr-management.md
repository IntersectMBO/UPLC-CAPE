# 2. Use Log4brains for ADR management

Date: 2025-07-17

## Status

Accepted

## Context

We need a tool to manage our Architecture Decision Records effectively. The
project requires a modern, maintainable solution for creating, organizing, and
presenting ADRs to the development team and community.

## Decision

We will use Log4brains as our ADR management tool instead of custom shell
scripts.

## Consequences

**Positive:**

- Modern web-based interface for browsing and creating ADRs
- Standardized ADR templates and workflows
- Better integration with modern development environments
- Active maintenance and community support
- Preview functionality for reviewing ADRs before committing

**Negative:**

- Requires Node.js in the development environment
- Additional dependency compared to simple shell scripts

## Implementation

Log4brains is integrated into our Nix development shell with convenient short
aliases:

- `adr new "Title"` - Create new ADR
- `adr preview` - Preview ADRs in browser
- `adr build` - Build static documentation site
- `adr help` - Show available commands

Single letter aliases are also available: `adr n`, `adr p`, `adr b`, etc.
