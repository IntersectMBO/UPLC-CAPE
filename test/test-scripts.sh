#!/usr/bin/env bash

# UPLC CAPE Test Suite
# Tests all CLI commands and scripts for proper functionality

set -uo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Temporary directory for test files
TEST_TMP_DIR=""

# Cleanup function
cleanup() {
  local exit_code=$?

  # Kill any background processes that might still be running
  jobs -p | xargs -r kill 2> /dev/null || true

  # Clean up temporary files
  if [[ -n "${TEST_TMP_DIR}" && -d "${TEST_TMP_DIR}" ]]; then
    echo "Cleaning up temporary directory: ${TEST_TMP_DIR}"
    rm -rf "${TEST_TMP_DIR}" 2> /dev/null || true
  fi

  # Clean up any test artifacts that might have been created
  rm -f scenarios/test-benchmark-* 2> /dev/null || true
  rm -f scenarios/test-interactive-* 2> /dev/null || true
  rm -f scenarios/test-workflow-* 2> /dev/null || true
  rm -rf submissions/*/TestCompiler_* 2> /dev/null || true
  rm -rf submissions/*/WorkflowComp_* 2> /dev/null || true

  # Print final results
  echo
  echo "=========================================="
  echo "Test Results:"
  echo "Tests run:    ${TESTS_RUN}"
  echo -e "Tests passed: ${GREEN}${TESTS_PASSED}${NC}"
  echo -e "Tests failed: ${RED}${TESTS_FAILED}${NC}"

  if [[ ${TESTS_FAILED} -eq 0 ]]; then
    echo -e "Result: ${GREEN}ALL TESTS PASSED${NC}"
  else
    echo -e "Result: ${RED}SOME TESTS FAILED${NC}"
  fi
  echo "=========================================="

  if [[ ${TESTS_FAILED} -gt 0 ]]; then
    exit 1
  fi

  exit $exit_code
}

# Set up cleanup trap
trap cleanup EXIT INT TERM

# Helper functions
log_info() {
  echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $1"
}

run_test() {
  local test_name="$1"
  local test_command="$2"
  local timeout_seconds="${3:-10}" # Default 10 second timeout

  ((TESTS_RUN++))
  echo
  echo "Running test: ${test_name}"
  echo "Command: ${test_command}"
  echo "Timeout: ${timeout_seconds}s"

  # Execute command with timeout and capture exit code
  local exit_code=0
  timeout "${timeout_seconds}s" bash -c "${test_command}" || exit_code=$?

  if [[ ${exit_code} -eq 0 ]]; then
    log_info "✓ ${test_name} PASSED"
    ((TESTS_PASSED++))
    return 0
  elif [[ ${exit_code} -eq 124 ]]; then
    log_error "✗ ${test_name} FAILED (TIMEOUT after ${timeout_seconds}s)"
    ((TESTS_FAILED++))
    return 1
  else
    log_error "✗ ${test_name} FAILED (exit code: ${exit_code})"
    ((TESTS_FAILED++))
    return 1
  fi
}

# Helper function for running commands that expect interactive input
run_test_with_input() {
  local test_name="$1"
  local test_command="$2"
  local input_text="$3"
  local timeout_seconds="${4:-10}"

  # Use echo to provide input and pipe to the command
  run_test "${test_name}" "echo '${input_text}' | ${test_command}" "${timeout_seconds}"
}

# Helper function for testing command exists and is executable
test_command_exists() {
  local test_name="$1"
  local command_path="$2"

  run_test "${test_name} exists" "test -f ${command_path}" 2
  run_test "${test_name} executable" "test -x ${command_path}" 2
}

# Setup test environment
setup_test_env() {
  log_info "Setting up test environment..."

  # Create temporary directory
  TEST_TMP_DIR=$(mktemp -d -t uplc-cape-test-XXXXXX)
  echo "Created temporary directory: ${TEST_TMP_DIR}"

  # Change to project root
  cd "$(dirname "$0")/.."

  # Verify we're in the right directory
  if [[ ! -f "README.md" || ! -d "scripts" ]]; then
    log_error "Not in UPLC-CAPE project root directory"
    exit 1
  fi

  # Verify cape command is available in PATH
  if ! command -v cape > /dev/null 2>&1; then
    log_error "cape command not found in PATH. Make sure you're in the" \
      "nix shell environment."
    exit 1
  fi

  log_info "Test environment setup complete"
}

# Test CAPE main command
test_cape_main() {
  log_info "Testing CAPE main command..."

  # Test CAPE help
  run_test "cape --help" "cape --help >/dev/null 2>&1" 5

  # Test CAPE with no args (should show help but exit with error code)
  run_test "cape (no args) exits with error" "! cape >/dev/null 2>&1" 5
}

# Test CAPE benchmark commands
test_cape_benchmark() {
  log_info "Testing CAPE benchmark commands..."

  # Test benchmark help
  run_test "cape benchmark --help" "cape benchmark --help >/dev/null 2>&1" 5

  # Test benchmark list with empty input (show all)
  run_test "cape benchmark list (show all)" "echo '' | cape benchmark list >/dev/null 2>&1" 5

  # Test benchmark with no subcommand (should default to list) with empty input
  run_test "cape benchmark (no subcommand)" "echo '' | cape benchmark >/dev/null 2>&1" 5

  # Test benchmark list with help
  run_test "cape benchmark list --help" "cape benchmark list --help >/dev/null 2>&1" 5

  # Test benchmark new help
  run_test "cape benchmark new --help" "cape benchmark new --help >/dev/null 2>&1" 5

  # Test creating a new benchmark with non-interactive input
  local test_benchmark_name="test-benchmark-$$"
  if [[ ! -f "scenarios/${test_benchmark_name}.md" ]]; then
    run_test "cape benchmark new (with name)" \
      "cape benchmark new ${test_benchmark_name} >/dev/null 2>&1" 10

    # Clean up the created benchmark if it exists
    if [[ -f "scenarios/${test_benchmark_name}.md" ]]; then
      rm -f "scenarios/${test_benchmark_name}.md"
      log_info "Cleaned up test benchmark: ${test_benchmark_name}"
    fi
  fi

  # Test showing a specific benchmark if fibonacci exists
  if [[ -f "scenarios/fibonacci.md" ]]; then
    run_test "cape benchmark fibonacci" "cape benchmark fibonacci >/dev/null 2>&1" 5
    run_test "cape benchmark list fibonacci" "cape benchmark list fibonacci >/dev/null 2>&1" 5
  else
    log_warn "Fibonacci scenario not found, skipping specific benchmark tests"
  fi
}

# Test CAPE submission commands
test_cape_submission() {
  log_info "Testing CAPE submission commands..."

  # Test submission help
  run_test "cape submission --help" "cape submission --help >/dev/null 2>&1" 5

  # Test submission list with empty input
  run_test "cape submission list" "echo '' | cape submission list >/dev/null 2>&1" 5

  # Test submission list help
  run_test "cape submission list --help" "cape submission list --help >/dev/null 2>&1" 5

  # Test submission new help
  run_test "cape submission new --help" "cape submission new --help >/dev/null 2>&1" 5

  # Test submission list for specific scenario if fibonacci exists
  if [[ -f "scenarios/fibonacci.md" ]]; then
    run_test "cape submission list fibonacci" \
      "cape submission list fibonacci >/dev/null 2>&1" 5
  fi
}

# Test error conditions
test_error_conditions() {
  log_info "Testing error conditions..."

  # Test invalid command
  run_test "cape invalid command (should fail)" "! cape invalid_command >/dev/null 2>&1" 5

  # Test invalid subcommand
  run_test "cape benchmark invalid (should fail)" \
    "! cape benchmark invalid_subcommand >/dev/null 2>&1" 5

  # Test invalid submission subcommand
  run_test "cape submission invalid (should fail)" \
    "! cape submission invalid_subcommand >/dev/null 2>&1" 5

  # Test nonexistent benchmark
  run_test "cape benchmark nonexistent (should fail)" \
    "! cape benchmark nonexistent_benchmark_name_12345 >/dev/null 2>&1" 5
}

# Test integration scenarios
test_integration() {
  log_info "Testing integration scenarios..."

  # Test full workflow: list benchmarks -> list submissions
  run_test "Integration: list benchmarks" "echo '' | cape benchmark list >/dev/null 2>&1" 5
  run_test "Integration: list submissions" "echo '' | cape submission list >/dev/null 2>&1" 5

  # Test specific benchmark workflow if fibonacci exists
  if [[ -f "scenarios/fibonacci.md" ]]; then
    run_test "Integration: show fibonacci benchmark" \
      "cape benchmark fibonacci >/dev/null 2>&1" 5
    run_test "Integration: list fibonacci submissions" \
      "cape submission list fibonacci >/dev/null 2>&1" 5
  fi
}

# Test directory structure validation
test_directory_structure() {
  log_info "Testing directory structure validation..."

  # Verify required directories exist
  run_test "scenarios directory exists" "test -d scenarios" 2
  run_test "submissions directory exists" "test -d submissions" 2
  run_test "scripts directory exists" "test -d scripts" 2
  run_test "scripts/cape-subcommands directory exists" "test -d scripts/cape-subcommands" 2

  # Verify TEMPLATE directories exist
  run_test "scenarios/TEMPLATE directory exists" "test -d scenarios/TEMPLATE" 2
  run_test "submissions/TEMPLATE directory exists" "test -d submissions/TEMPLATE" 2

  # Verify key subcommand scripts exist
  run_test "benchmark list script exists" "test -f scripts/cape-subcommands/benchmark/list.sh" 2
  run_test "benchmark new script exists" "test -f scripts/cape-subcommands/benchmark/new.sh" 2
  run_test "submission list script exists" \
    "test -f scripts/cape-subcommands/submission/list.sh" 2
  run_test "submission new script exists" \
    "test -f scripts/cape-subcommands/submission/new.sh" 2
}

# Test output formats and validation
test_output_formats() {
  log_info "Testing output formats and command discovery..."

  # Test that help outputs contain expected content
  run_test "cape help contains 'benchmark'" "cape --help | grep -q benchmark" 5
  run_test "cape help contains 'submission'" "cape --help | grep -q submission" 5

  # Test benchmark list output format
  local benchmark_output="${TEST_TMP_DIR}/benchmark_list.txt"
  if timeout 5s bash -c "echo '' | cape benchmark list > '${benchmark_output}' 2>&1"; then
    run_test "benchmark list produces output" "test -s ${benchmark_output}" 2
  fi

  # Test submission list output format
  local submission_output="${TEST_TMP_DIR}/submission_list.txt"
  if timeout 5s bash -c "echo '' | cape submission list > '${submission_output}' 2>&1"; then
    run_test "submission list produces output" "test -s ${submission_output}" 2
  fi
}

# Test CAPE submission creation commands
test_cape_submission_creation() {
  log_info "Testing CAPE submission creation commands..."

  # Test submission new with all explicit arguments
  local test_scenario="fibonacci"
  local test_compiler="TestCompiler"
  local test_version="1.0.0"
  local test_handle="testuser"
  local test_submission_dir="submissions/${test_scenario}/${test_compiler}_${test_version}_${test_handle}"

  if [[ -f "scenarios/${test_scenario}.md" ]]; then
    # Test with all explicit arguments
    run_test "cape submission new (all explicit args)" \
      "cape submission new ${test_scenario} ${test_compiler} ${test_version} ${test_handle} >/dev/null 2>&1" 15

    # Verify submission directory was created
    if [[ -d "${test_submission_dir}" ]]; then
      run_test "submission directory created" "test -d ${test_submission_dir}" 2
      run_test "submission UPLC file created" \
        "test -f ${test_submission_dir}/${test_scenario}.uplc" 2
      run_test "submission metrics.json created" \
        "test -f ${test_submission_dir}/metrics.json" 2
      run_test "submission metadata.json created" \
        "test -f ${test_submission_dir}/metadata.json" 2
      run_test "submission README.md created" \
        "test -f ${test_submission_dir}/README.md" 2
      run_test "submission source directory created" \
        "test -d ${test_submission_dir}/source" 2
      run_test "submission config.json created" \
        "test -f ${test_submission_dir}/config.json" 2

      # Clean up
      rm -rf "${test_submission_dir}"
      log_info "Cleaned up test submission: ${test_submission_dir}"
    else
      log_error "Expected submission directory was not created: ${test_submission_dir}"
      ((TESTS_FAILED++))
    fi

    # Test with interactive input (provide all inputs via echo)
    local input_data="${test_scenario}\n${test_compiler}Interactive\n${test_version}\n${test_handle}interactive"
    local interactive_submission_dir="submissions/${test_scenario}/${test_compiler}Interactive_${test_version}_${test_handle}interactive"

    run_test "cape submission new (interactive)" \
      "printf '${input_data}' | cape submission new >/dev/null 2>&1" 15

    # Verify interactive submission was created
    if [[ -d "${interactive_submission_dir}" ]]; then
      run_test "interactive submission directory created" \
        "test -d ${interactive_submission_dir}" 2

      # Clean up
      rm -rf "${interactive_submission_dir}"
      log_info "Cleaned up interactive test submission: ${interactive_submission_dir}"
    fi

    # Test with partial arguments (should prompt for missing ones)
    local partial_input_data="${test_version}\n${test_handle}partial"
    local partial_submission_dir="submissions/${test_scenario}/${test_compiler}_${test_version}_${test_handle}partial"

    run_test "cape submission new (partial args)" \
      "printf '${partial_input_data}' | cape submission new ${test_scenario} ${test_compiler} >/dev/null 2>&1" 15

    # Verify partial submission was created
    if [[ -d "${partial_submission_dir}" ]]; then
      run_test "partial submission directory created" "test -d ${partial_submission_dir}" 2

      # Clean up
      rm -rf "${partial_submission_dir}"
      log_info "Cleaned up partial test submission: ${partial_submission_dir}"
    fi
  else
    log_warn "Fibonacci scenario not found, skipping submission creation tests"
  fi

  # Test error conditions for submission creation
  run_test "cape submission new nonexistent scenario (should fail)" \
    "! cape submission new nonexistent_scenario TestComp 1.0 user >/dev/null 2>&1" 10

  # Test invalid input handling (empty inputs should fail)
  run_test "cape submission new empty inputs (should fail)" \
    "! printf '\n\n\n\n' | cape submission new >/dev/null 2>&1" 10
}

# Test CAPE benchmark creation commands
test_cape_benchmark_creation() {
  log_info "Testing CAPE benchmark creation commands..."

  # Test benchmark new with explicit argument
  local test_benchmark_name="test-benchmark-$$"
  local test_benchmark_file="scenarios/${test_benchmark_name}.md"

  # Test with explicit benchmark name
  run_test "cape benchmark new (explicit name)" \
    "cape benchmark new ${test_benchmark_name} >/dev/null 2>&1" 15

  # Verify benchmark file was created
  if [[ -f "${test_benchmark_file}" ]]; then
    run_test "benchmark file created" "test -f ${test_benchmark_file}" 2
    run_test "benchmark file not empty" "test -s ${test_benchmark_file}" 2

    # Verify content was templated correctly
    run_test "benchmark contains scenario name" \
      "grep -q '${test_benchmark_name}' ${test_benchmark_file}" 2

    # Clean up
    rm -f "${test_benchmark_file}"
    log_info "Cleaned up test benchmark: ${test_benchmark_file}"
  else
    log_error "Expected benchmark file was not created: ${test_benchmark_file}"
    ((TESTS_FAILED++))
  fi

  # Test with interactive input
  local interactive_benchmark_name="test-interactive-$$"
  local interactive_benchmark_file="scenarios/${interactive_benchmark_name}.md"

  run_test "cape benchmark new (interactive)" \
    "printf '${interactive_benchmark_name}\n' | cape benchmark new >/dev/null 2>&1" 15

  # Verify interactive benchmark was created
  if [[ -f "${interactive_benchmark_file}" ]]; then
    run_test "interactive benchmark file created" "test -f ${interactive_benchmark_file}" 2

    # Clean up
    rm -f "${interactive_benchmark_file}"
    log_info "Cleaned up interactive test benchmark: ${interactive_benchmark_file}"
  fi

  # Test error conditions for benchmark creation
  run_test "cape benchmark new invalid name (should fail)" \
    "! cape benchmark new Invalid-Name-123 >/dev/null 2>&1" 10
  run_test "cape benchmark new uppercase (should fail)" \
    "! cape benchmark new UpperCase >/dev/null 2>&1" 10
  run_test "cape benchmark new starts with number (should fail)" \
    "! cape benchmark new 123invalid >/dev/null 2>&1" 10
  run_test "cape benchmark new starts with hyphen (should fail)" \
    "! cape benchmark new -invalid >/dev/null 2>&1" 10
  run_test "cape benchmark new ends with hyphen (should fail)" \
    "! cape benchmark new invalid- >/dev/null 2>&1" 10

  # Test duplicate benchmark creation (should fail)
  if [[ -f "scenarios/fibonacci.md" ]]; then
    run_test "cape benchmark new duplicate (should fail)" \
      "! cape benchmark new fibonacci >/dev/null 2>&1" 10
  fi

  # Test empty input handling (should fail)
  run_test "cape benchmark new empty input (should fail)" \
    "! printf '\n' | cape benchmark new >/dev/null 2>&1" 10
}

# Test creation workflow integration
test_creation_workflow_integration() {
  log_info "Testing creation workflow integration..."

  # Test complete workflow: create benchmark -> create submission
  local workflow_benchmark="test-workflow-$$"
  local workflow_benchmark_file="scenarios/${workflow_benchmark}.md"
  local workflow_compiler="WorkflowComp"
  local workflow_version="2.0.0"
  local workflow_handle="workflowuser"
  local workflow_submission_dir="submissions/${workflow_benchmark}/${workflow_compiler}_${workflow_version}_${workflow_handle}"

  # Step 1: Create benchmark
  run_test "Workflow: create benchmark" \
    "cape benchmark new ${workflow_benchmark} >/dev/null 2>&1" 15

  if [[ -f "${workflow_benchmark_file}" ]]; then
    # Step 2: Create submission for the new benchmark
    run_test "Workflow: create submission for new benchmark" \
      "cape submission new ${workflow_benchmark} ${workflow_compiler} ${workflow_version} ${workflow_handle} >/dev/null 2>&1" 15

    if [[ -d "${workflow_submission_dir}" ]]; then
      run_test "Workflow: submission created for new benchmark" \
        "test -d ${workflow_submission_dir}" 2

      # Verify the submission references the correct benchmark
      run_test "Workflow: submission UPLC file matches benchmark" \
        "test -f ${workflow_submission_dir}/${workflow_benchmark}.uplc" 2

      # Clean up submission
      rm -rf "${workflow_submission_dir}"
      log_info "Cleaned up workflow test submission: ${workflow_submission_dir}"
    fi

    # Clean up benchmark
    rm -f "${workflow_benchmark_file}"
    log_info "Cleaned up workflow test benchmark: ${workflow_benchmark_file}"
  fi
}

# Main test execution
main() {
  echo "UPLC CAPE Test Suite"
  echo "===================="
  echo

  setup_test_env

  log_info "Running complete test suite..."

  # Core CAPE command tests
  test_cape_main
  test_cape_benchmark
  test_cape_submission

  # Creation command tests
  test_cape_submission_creation
  test_cape_benchmark_creation
  test_creation_workflow_integration

  # Directory structure tests
  test_directory_structure

  # Error condition tests
  test_error_conditions

  # Integration tests
  test_integration

  # Output format tests
  test_output_formats

  # Submission and benchmark creation tests
  test_cape_submission_creation
  test_cape_benchmark_creation

  # Creation workflow integration tests
  test_creation_workflow_integration

  log_info "All tests completed!"
}

# Show usage if help is requested
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  echo "Usage: $0"
  echo ""
  echo "Runs the complete UPLC CAPE test suite."
  echo "This script must be run from within the nix development shell."
  echo ""
  echo "Example:"
  echo "  $0          # Run full test suite"
  echo "  $0 --help   # Show this help"
  exit 0
fi

# Run main function
main "$@"
