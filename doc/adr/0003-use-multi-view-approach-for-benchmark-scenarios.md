# Use Multi-View Approach for Benchmark Scenario Specification

- Status: accepted
- Date: 2025-01-22
- Tags: specification, documentation, benchmarks, architecture

Technical Story: Refactor the monolithic fibonacci benchmark template by extracting individual files for different view types (state machine, behavioral scenarios, implementation logic).

## Context and Problem Statement

The initial fibonacci benchmark scenario was specified as a single monolithic document that mixed different types of specification concerns: lifecycle visualization, behavioral test cases, and algorithmic implementation details. This approach created several issues:

1. **Cognitive overload**: Developers had to parse through different types of information to find what they needed
2. **Maintenance complexity**: Changes to one aspect (e.g., test scenarios) required editing a large document containing unrelated concerns
3. **Poor reusability**: Different stakeholders needed different views but had to consume the entire specification
4. **Inconsistent abstraction levels**: The document mixed high-level state transitions with low-level implementation details

How can we structure benchmark scenario specifications to be more maintainable, consumable, and aligned with best practices for smart contract specification?

## Decision Drivers

- Need for clear separation of concerns in specification documents
- Research findings advocating for multi-view specification approaches for smart contracts
- Desire to make scenarios argument-agnostic and reusable across different test cases
- Goal to improve maintainability and reduce cognitive load for different stakeholder groups
- Alignment with established software engineering practices for specification documentation

## Considered Options

1. **Single monolithic document** (current approach)
2. **Multi-view approach with three distinct views**
3. **Fully formal specification using mathematical notation**
4. **Code-first approach with minimal documentation**

## Decision Outcome

Chosen option: "Multi-view approach with three distinct views", because it directly addresses the identified problems while following research-backed best practices for smart contract specification.

### Positive Consequences

- **Improved maintainability**: Each view can be updated independently without affecting others
- **Better stakeholder alignment**: Different roles can focus on their relevant view (auditors on behavioral scenarios, implementers on logic view, etc.)
- **Enhanced reusability**: Views are argument-agnostic and can be applied to different test cases
- **Clearer documentation structure**: Each view has a specific purpose and abstraction level
- **Reduced cognitive load**: Stakeholders can consume only the information relevant to their needs

### Negative Consequences

- **Increased file count**: Three files instead of one per scenario
- **Cross-reference complexity**: Need to maintain consistency across multiple views
- **Initial learning curve**: Contributors need to understand the multi-view approach

## Pros and Cons of the Options

### Single monolithic document

Current approach where all specification aspects are in one file.

- Good, because simple file structure with everything in one place
- Good, because no cross-reference management needed
- Bad, because creates cognitive overload for readers
- Bad, because difficult to maintain as scenarios grow in complexity
- Bad, because mixes different abstraction levels inappropriately

### Multi-view approach with three distinct views

Three separate files: state machine view, behavioral scenarios view, and implementation logic view.

- Good, because aligns with established specification practices for smart contract development
- Good, because provides clear separation of concerns
- Good, because enables stakeholder-specific consumption
- Good, because improves maintainability through modular structure
- Good, because makes scenarios argument-agnostic and more reusable
- Bad, because increases file count and cross-reference complexity
- Bad, because requires initial learning investment for contributors

### Fully formal specification using mathematical notation

Mathematical specification using formal methods notation.

- Good, because provides unambiguous specification
- Good, because enables formal verification
- Bad, because requires specialized knowledge to read and write
- Bad, because not accessible to most smart contract developers
- Bad, because overkill for benchmark scenario specification

### Code-first approach with minimal documentation

Minimal documentation with primary specification in reference implementations.

- Good, because executable specification that can't drift from implementation
- Good, because familiar to developers
- Bad, because ties specification to specific programming languages
- Bad, because makes cross-compiler comparison more difficult
- Bad, because reduces accessibility for non-implementers (auditors, stakeholders)

## Implementation Details

The multi-view approach consists of three distinct files per scenario:

### 1. State Machine View (`*-state-machine.md`)

- **Purpose**: High-level lifecycle visualization
- **Content**: Mermaid state diagrams embedded in markdown, state descriptions, transition conditions
- **Audience**: All stakeholders seeking high-level understanding

### 2. Behavioral Scenarios View (`*-behavioral-scenarios.md`)

- **Purpose**: Concrete test cases and expected outcomes
- **Content**: Gherkin scenarios covering happy paths and edge cases
- **Audience**: Testers, auditors, quality assurance

### 3. Implementation Logic View (`*-implementation-logic.md`)

- **Purpose**: Algorithmic specification for implementers
- **Content**: Language-agnostic pseudocode, complexity analysis, algorithmic approaches
- **Audience**: Compiler authors, implementers

All views are kept argument-agnostic (no hardcoded values like "fibonacci(25)") to maximize reusability across different test cases.

## Links

- See [Fibonacci Scenario](../../scenarios/fibonacci.md) for the first implementation of this approach
