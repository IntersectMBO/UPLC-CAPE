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

---

## View 1: State Lifecycle View

The {Scenario Name} program operates as a **{description of execution model}**:

```
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

## View 2: Behavioral Scenario View

### Core Algorithm

```
{pseudo_code_or_algorithm_description}
```

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

### Performance Characteristics

**Expected Ranges** (approximate guidelines for reference implementations):

- CPU Units: {min_cpu} - {max_cpu}
- Memory Units: {min_mem} - {max_mem}
- Script Size: {min_size} - {max_size} bytes

_Note: These ranges are indicative and may vary significantly based on compiler optimizations and implementation approaches._

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

Use the standard metrics template in `submissions/TEMPLATE/metrics-template.json`:

```json
{
  "scenario": "{scenario_name}",
  "version": "1.0.0",
  "measurements": {
    "cpu_units": "<measured_value>",
    "memory_units": "<measured_value>",
    "script_size_bytes": "<measured_value>",
    "term_size": "<measured_value>"
  },
  "execution_environment": {
    "evaluator": "<evaluator_version>"
  },
  "timestamp": "<ISO-8601_timestamp>",
  "measurement_method": "manual",
  "notes": "<optional_implementation_notes>"
}
```

---

## Implementation Notes

### Algorithm Considerations

- {Implementation guidance 1}
- {Implementation guidance 2}
- {Performance optimization suggestions}

### Common Pitfalls

- {Pitfall 1}: {description and how to avoid}
- {Pitfall 2}: {description and how to avoid}

### Optimization Opportunities

- {Optimization 1}: {description and potential benefits}
- {Optimization 2}: {description and potential benefits}

---

## References and Resources

### Related Work

- {Reference 1}: {description}
- {Reference 2}: {description}

### Helpful Documentation

- [UPLC Specification]({link})
- [CEK Machine Documentation]({link})
- [Cardano Protocol Parameters]({link})

---

## Changelog

### Version 1.0.0 (Initial)

- Initial scenario specification
- Defined core requirements and constraints
- Established measurement guidelines

---

_This scenario is part of the CAPE (Comparative Artifact Performance Evaluation) framework for benchmarking UPLC compiler performance._
