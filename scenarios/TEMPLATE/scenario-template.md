# {Scenario Name} Scenario

## Overview

The {Scenario Name} benchmark is a **{synthetic/real-world} {type}** designed to measure the performance characteristics of {description} implemented as UPLC programs. This benchmark tests a compiler's ability to {specific capabilities being tested}.

**Purpose**: This scenario serves as a standardized test for measuring:

- {Metric 1} for {specific aspect}
- {Metric 2} during {specific operations}
- {Metric 3} optimization for {specific algorithms/patterns}
- {Metric 4} efficiency in {specific encoding/representation}

**Key Constraint**: The UPLC program must be **fully-applied** before benchmarking, meaning {specific constraint explanation}. This ensures consistent measurement across all compiler implementations and prevents variations based on different {parameter/input} values.

**Target Computation**: `{computation_description}({parameters}) = {expected_result}`

- This {value/computation} is chosen to be {rationale for choice}
- It fits comfortably within the CEK machine execution budget limits
- It provides sufficient {complexity/depth} to test {optimization capabilities}

**Success Criteria**: The program must successfully {success condition} within the execution budget constraints.

## Evaluation Modes

This scenario supports two evaluation modes to serve different benchmarking needs:

### Base Mode (Compiler Comparison)

**Purpose**: Pure compiler-to-compiler comparison with fixed algorithm

All "base" mode submissions MUST implement the prescribed algorithm below:

```haskell
{function_name} :: {type_signature}
{function_implementation}
  -- Implementation details
```

**Requirements:**

- Implement exactly the algorithm specified above
- No algorithmic optimizations beyond compiler's automatic optimizations
- {Specific constraint 1}
- {Specific constraint 2}

**Directory naming**: `submissions/{scenario_name}/{Compiler}_{Version}_{Author}_base/`

**Use case**: Academic research, tracking compiler optimization improvements over versions

### Open Mode (Real-World Competition)

**Purpose**: Showcase compiler ecosystem capabilities with any optimization technique

**Allowed**:

- Any algorithmic approach (recursive, iterative, memoized, etc.)
- Metaprogramming and code generation techniques
- Any compiler-specific optimization features
- Multiple submissions per compiler/author using unique slugs

**Directory naming**:

- Generic implementation: `submissions/{scenario_name}/{Compiler}_{Version}_{Author}_open/`
- Specific optimization: `submissions/{scenario_name}/{Compiler}_{Version}_{Author}_open_{slug}/`

**Slug examples** (optional): `memoized`, `unrolled`, `optimal1`, `metaprogrammed`

**Use case**: Practical developer guidance, demonstrating compiler USPs

### Mode Selection

When creating a submission:

```bash
# Base mode (prescribed algorithm)
cape submission new {scenario_name} MyCompiler 1.0.0 handle --mode base

# Open mode (generic/default optimization)
cape submission new {scenario_name} MyCompiler 1.0.0 handle --mode open

# Open mode (specific optimization with slug)
cape submission new {scenario_name} MyCompiler 1.0.0 handle --mode open --slug memoized
```

Reports show both modes by default, or filter with `--mode base` or `--mode open`.

**Note**: If this scenario does not define a base mode algorithm, only open mode is available.

## TL;DR

Implement a {brief_description} and compile it as a fully-applied UPLC program.

**Required Files**: Submit `{scenario_name}.uplc`, `metadata.json`, `metrics.json` to `submissions/{scenario_name}/{Compiler}_{Version}_{Handle}/`

**Target**: {target_computation} → Expected result: `{expected_result}`  
**Metrics**: CPU units, Memory units, Script size (bytes), Term size  
**Constraints**: Plutus Core 1.1.0, CEK machine budget limits  
**Implementation**: {implementation_summary}

---

## Exact Task

Implement a {detailed_task_description} and compile it as a **fully-applied UPLC program** that {specific_functionality}.

### Core Requirements

1. **{Primary_Component}**: Create a {component_description} with signature `{function_signature}` that handles:
   - `{Operation_1}` ({parameter} = {value}): {description_1}
   - `{Operation_2}` ({parameter} = {value}): {description_2}
   - `{Operation_3}` ({parameter} = {value}): {description_3}

2. **Fixed Parameters**: The following parameters must be baked into the UPLC program:
   - **{Parameter_1}**: `{value_1}`
   - **{Parameter_2}**: `{value_2}`
   - **{Parameter_3}**: `{value_3}`

3. **{Validation_Rules}**: The {component} must enforce {specific_constraints}

### {Input_Format} Encoding

```text
{Format_1} = {value_1}  -- {description_1}
{Format_2} = {value_2}  -- {description_2}
{Format_3} = {value_3}  -- {description_3}
```

### Test Context Assumptions

When testing this {component}, the test framework uses {framework_description}:

**{Context_Component_1}:**

- **{Field_1}**: `{value_1}` ({description_1})
- **{Field_2}**: `{value_2}`
- **{Field_3}**: `{value_3}`

**{Context_Component_2}:**

- {Assumption_1}
- {Assumption_2}
- {Assumption_3}

**Test Framework Behavior:**

- {Behavior_1}
- {Behavior_2}
- {Behavior_3}

---

## View 1: State Lifecycle View

The {Scenario Name} program operates as a **{description of execution model}**:

```text
[Start] --{operation}()--> [{Result State}: {expected_result}]
```

| Current State | Event | Condition | Next State |
| --- | --- | --- | --- |
| **Start** | `{initial_event}()` | Program starts execution | **{Processing_State}** |
| **{Processing_State}** | `{processing_event}()` | {processing_condition} | **{Processing_State}** (until {termination_condition}) |
| **{Processing_State}** | `{completion_event}()` | {completion_condition} | **Result** |
| **Result** | - | Result = {expected_result} | **Complete** |

**State Descriptions**:

- **Start**: Program begins execution with the {input description}
- **{Processing_State}**: {Description of processing phase}
- **Result**: {Description of result computation}
- **Complete**: Program execution terminates successfully

**Note**: {Any additional notes about the execution model or failure states}

---

## View 2: Transaction Sequence View

### {Sequence_Type} Flow Diagram

```mermaid
{mermaid_diagram_placeholder}
```

### Performance Measurement Sequences

**{Primary_Sequence}**:

1. **{Step_1}**: {description_1}
2. **{Step_2}**: {description_2}
3. **{Step_3}**: {description_3}

**Performance Measurement**: {measurement_strategy}

### Input Specification

**Fixed Parameters**:

- `{parameter1}`: {value} - {rationale}
- `{parameter2}`: {value} - {rationale}

**Expected Behavior**:

- The program must {behavior requirement 1}
- The program must {behavior requirement 2}
- The program must {behavior requirement 3}

---

## Implementation Requirements

### Technical Constraints

1. **Execution Budget**: The program must complete within standard CEK machine limits
2. **Determinism**: Results must be identical across multiple executions
3. **Self-Contained**: All parameters must be baked into the UPLC program
4. **Correctness**: Must produce the exact expected result: `{expected_result}`

### Validation

To validate your implementation:

1. **Functional Test**: Ensure the program outputs `{expected_result}`
2. **Budget Test**: Verify execution completes within CEK machine limits
3. **Determinism Test**: Run multiple times to confirm consistent results

---

## Test Cases

### Primary Test Case

**Input**: {input_description} **Expected Output**: `{expected_result}` **Validation Method**: {how_to_validate}

### Edge Case Considerations

- {Edge case 1}: {description and expected behavior}
- {Edge case 2}: {description and expected behavior}

---

## Measurement Guidelines

### Required Metrics

All submissions must include measurements for:

1. **CPU Units**: Total computational cost
2. **Memory Units**: Peak memory consumption
3. **Script Size**: Size of the compiled UPLC script in bytes
4. **Term Size**: Size of the UPLC term representation

### Measurement Environment

- **Evaluator**: Use the standard CEK machine evaluator
- **Measurement Method**: Document how metrics were obtained

### Reporting Format

Use the standard metrics schema as defined in `submissions/TEMPLATE/metrics.schema.json`:

```json
{
  "scenario": "{scenario_name}",
  "version": "1.0.0",
  "measurements": {
    "cpu_units": {
      "maximum": 0,
      "sum": 0,
      "minimum": 0,
      "median": 0,
      "sum_positive": 0,
      "sum_negative": 0
    },
    "memory_units": {
      "maximum": 0,
      "sum": 0,
      "minimum": 0,
      "median": 0,
      "sum_positive": 0,
      "sum_negative": 0
    },
    "script_size_bytes": 0,
    "term_size": 0
  },
  "evaluations": [
    {
      "name": "{test_case_name}",
      "description": "{what_this_test_validates}",
      "cpu_units": 0,
      "memory_units": 0,
      "execution_result": "success"
    }
  ],
  "execution_environment": {
    "evaluator": "<evaluator_version>"
  },
  "timestamp": "<ISO-8601_timestamp>",
  "notes": "<optional_implementation_notes>"
}
```

**Field Explanations:**

**Measurements Object**: Contains aggregated performance metrics across all test evaluations:

- **cpu_units/memory_units objects**: Multiple aggregation strategies for comprehensive analysis:
  - `maximum`: Peak resource usage (worst-case performance)
  - `sum`: Total resources across all evaluations (overall computational work)
  - `minimum`: Best-case resource usage (optimal performance)
  - `median`: Typical resource usage (normal performance)
  - `sum_positive`: Resources from successful evaluations only
  - `sum_negative`: Resources from failed evaluations only

- **script_size_bytes**: Size of the compiled UPLC validator script in bytes
- **term_size**: Number of AST nodes in the UPLC term representation

**Evaluations Array**: Individual test case measurements showing per-evaluation performance data. Each evaluation includes:

- `name`: Test case identifier
- `description`: What the test validates
- `cpu_units/memory_units`: Resources consumed for this specific test
- `execution_result`: "success" for validation pass, "error" for validation failure

**Environment Info**:

- `evaluator`: Tool/version used for UPLC evaluation
- `timestamp`: ISO-8601 timestamp when measurements were taken
- `notes`: Optional implementation details or measurement context

---

## References and Resources

### Helpful Documentation

- [UPLC Specification]({link})
- [CEK Machine Documentation]({link})
- [Cardano Protocol Parameters]({link})

---

_This scenario is part of the CAPE (Comparative Artifact Performance Evaluation) framework for benchmarking UPLC compiler performance._
