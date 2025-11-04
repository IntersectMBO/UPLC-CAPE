# Benchmark Templates

This directory contains template files for creating new benchmark scenarios in the CAPE framework.

## Files

- `scenario-template.md`: Template for creating new benchmark specifications

## Usage

To create a new benchmark:

```bash
cape benchmark new <benchmark-name>
```

This will:

1. Validate the benchmark name format (lowercase, hyphens allowed)
2. Create a new benchmark file from the template
3. Replace basic placeholders with the benchmark name
4. Provide guidance for completing the specification

## Benchmark Naming Convention

Benchmark names should be:

- Lowercase letters only
- Can contain underscores to separate words
- Descriptive of the computational task
- Start and end with a letter or number

**Valid examples:**

- `fibonacci`
- `two_party_escrow`
- `dao_voting`
- `merkle_proof`
- `nft_minting`

**Invalid examples:**

- `TwoParty` (uppercase)
- `two-party` (hyphen)
- `2escrow` (starts with number)
- `_escrow` (starts with underscore)
- `escrow_` (ends with underscore)

## Template Structure

The benchmark template includes:

1. **Overview**: Description and purpose
2. **State Lifecycle View**: Execution model and state transitions
3. **Behavioral Scenario View**: Algorithm and input specifications
4. **Implementation Requirements**: Technical constraints and validation
5. **Test Cases**: Primary test case and edge cases
6. **Measurement Guidelines**: Metrics and reporting format
7. **Implementation Notes**: Guidance and optimization tips

## Completing a Benchmark

When creating a new benchmark, replace all `{placeholder}` values with concrete specifications:

- `{Scenario Name}`: Human-readable title
- `{scenario_name}`: Machine-readable identifier
- `{synthetic/real-world}`: Type classification
- `{description}`: Computational task description
- `{expected_result}`: Target computation result
- `{parameter}`: Input parameters and constraints

## Guidelines

### Technical Requirements

- Define a specific, deterministic computation
- Ensure the result is verifiable and consistent
- Specify fully-applied UPLC constraints
- Include performance measurement guidelines
- Verify budget constraints fit CEK machine limits

### Documentation Quality

- Provide clear, unambiguous specifications
- Include examples and test cases
- Document edge cases and failure modes
- Add implementation guidance and optimization tips
- Reference related work and resources

### Community Process

- Share draft benchmarks for feedback
- Incorporate community suggestions
- Create reference implementations
- Document lessons learned and best practices

## Related Documentation

- [Domain Model](../../doc/domain-model.md) - Framework entities and relationships
- [Contributing Guide](../../CONTRIBUTING.md) - How to contribute benchmarks and submissions
- [Benchmark List](../) - Available benchmark scenarios
