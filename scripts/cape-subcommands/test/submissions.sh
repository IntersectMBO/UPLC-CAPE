#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

## CAPE Test Submissions - UPLC Validation for All Submissions

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"
# Always use the actual git repository root for building, not the sandbox
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# Parse help options
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Parse submission path arguments
SPECIFIC_SUBMISSIONS=()
while [[ $# -gt 0 ]]; do
  case $1 in
    -h | --help)
      cape_render_help "${BASH_SOURCE[0]}"
      exit 0
      ;;
    -*)
      cape_error "Unknown option: $1"
      exit 1
      ;;
    *)
      SPECIFIC_SUBMISSIONS+=("$1")
      shift
      ;;
  esac
done

# Test environment setup - support both standalone and integrated mode
if [[ -z "${TEST_TMP_DIR:-}" ]]; then
  # Standalone mode - use actual project directories
  SANDBOX_DIR="$PROJECT_ROOT"
  TEST_TMP_DIR="$(mktemp -d)"
  trap 'rm -rf "$TEST_TMP_DIR"' EXIT
  STANDALONE_MODE=true
else
  # Integrated mode - called from main test suite
  if [[ -z "${SANDBOX_DIR:-}" ]]; then
    cape_error "SANDBOX_DIR not set. This command should be run from the main test suite."
    exit 1
  fi
  STANDALONE_MODE=false
fi

# Test counters (expected to be set by parent)
TESTS_RUN="${TESTS_RUN:-0}"
TESTS_PASSED="${TESTS_PASSED:-0}"
TESTS_FAILED="${TESTS_FAILED:-0}"

# Declare global counters for validation results
declare -g failed_validations=0
declare -g total_validations=0

# Run UPLC validation for specific or all submissions
run_uplc_validation() {
  failed_validations=0
  total_validations=0

  if [[ ${#SPECIFIC_SUBMISSIONS[@]} -gt 0 ]]; then
    cape_info "Testing specific UPLC submissions..."
    run_specific_submissions
  else
    cape_info "Testing UPLC cape-tests validation..."
    run_all_submissions
  fi

  # Update global test counters
  TESTS_RUN=$((TESTS_RUN + total_validations))
  TESTS_PASSED=$((TESTS_PASSED + total_validations - failed_validations))
  TESTS_FAILED=$((TESTS_FAILED + failed_validations))

  if [[ $failed_validations -gt 0 ]]; then
    cape_error "$failed_validations of $total_validations UPLC validations failed"
  else
    cape_success "All $total_validations UPLC validations passed"
  fi

  # Write updated counters to a temporary file for parent to source
  if [[ -n "${COUNTER_UPDATE_FILE:-}" ]]; then
    {
      echo "TESTS_RUN=$TESTS_RUN"
      echo "TESTS_PASSED=$TESTS_PASSED"
      echo "TESTS_FAILED=$TESTS_FAILED"
    } > "$COUNTER_UPDATE_FILE"
  fi

  # Show summary in standalone mode
  if [[ "${STANDALONE_MODE:-false}" == "true" ]]; then
    echo ""
    echo "========================================"
    echo "Tests: $total_validations | Passed: $((total_validations - failed_validations)) | Failed: $failed_validations"
    if [[ $failed_validations -eq 0 ]]; then
      cape_success "ALL UPLC VALIDATIONS PASSED"
    fi
  fi

  return $failed_validations
}

# Run validation for all discovered submissions
run_all_submissions() {
  # Find all scenarios with cape-tests.json
  for scenario_tests in "$SANDBOX_DIR"/scenarios/*/cape-tests.json; do
    if [[ -f "$scenario_tests" ]]; then
      local scenario=$(basename "$(dirname "$scenario_tests")")

      # Find all UPLC submissions for this scenario
      for uplc_file in "$SANDBOX_DIR"/submissions/"$scenario"/*/*.uplc; do
        if [[ -f "$uplc_file" ]]; then
          local submission=$(basename "$(dirname "$uplc_file")")
          local test_name="${scenario}/${submission}"
          run_single_validation "$uplc_file" "$scenario_tests" "$test_name"
        fi
      done
    fi
  done
}

# Run validation for specific submission paths
run_specific_submissions() {
  for submission_path in "${SPECIFIC_SUBMISSIONS[@]}"; do
    # Normalize path - remove trailing slashes and make relative to SANDBOX_DIR
    submission_path="${submission_path%/}"

    # If path doesn't start with submissions/, assume it's relative to submissions/
    if [[ "$submission_path" != submissions/* ]]; then
      submission_path="submissions/$submission_path"
    fi

    local full_submission_path="$SANDBOX_DIR/$submission_path"

    if [[ ! -d "$full_submission_path" ]]; then
      cape_error "Submission directory not found: $submission_path"
      failed_validations=$((failed_validations + 1))
      total_validations=$((total_validations + 1))
      continue
    fi

    # Extract scenario from submission path
    local scenario_pattern="$SANDBOX_DIR/submissions/([^/]+)/"
    if [[ "$full_submission_path" =~ $scenario_pattern ]]; then
      local scenario="${BASH_REMATCH[1]}"
      local scenario_tests="$SANDBOX_DIR/scenarios/$scenario/cape-tests.json"

      if [[ ! -f "$scenario_tests" ]]; then
        cape_error "No cape-tests.json found for scenario: $scenario"
        failed_validations=$((failed_validations + 1))
        total_validations=$((total_validations + 1))
        continue
      fi

      # Find UPLC file in submission directory
      local uplc_file
      uplc_file=$(find "$full_submission_path" -name "*.uplc" -type f | head -1)

      if [[ -z "$uplc_file" ]]; then
        cape_error "No UPLC file found in submission: $submission_path"
        failed_validations=$((failed_validations + 1))
        total_validations=$((total_validations + 1))
        continue
      fi

      local submission_name=$(basename "$full_submission_path")
      local test_name="${scenario}/${submission_name}"
      run_single_validation "$uplc_file" "$scenario_tests" "$test_name"
    else
      cape_error "Invalid submission path format: $submission_path"
      failed_validations=$((failed_validations + 1))
      total_validations=$((total_validations + 1))
    fi
  done
}

# Run validation for a single submission
run_single_validation() {
  local uplc_file="$1"
  local scenario_tests="$2"
  local test_name="$3"

  total_validations=$((total_validations + 1))

  # Use longer timeout in CI environments
  local timeout_duration="60s"
  if [[ "${CI:-}" == "true" ]] || [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then
    timeout_duration="180s"
  fi

  if (cd "$REPO_ROOT" && timeout "$timeout_duration" cabal run measure -- --validate-only -i "$(realpath --relative-to="$REPO_ROOT" "$uplc_file")" -t "$(realpath --relative-to="$REPO_ROOT" "$scenario_tests")" -o /tmp/temp-metrics.json > /dev/null 2>&1); then
    cape_success "✓ $test_name"
  else
    cape_error "✗ $test_name"
    failed_validations=$((failed_validations + 1))
  fi
}

# Main execution
main() {
  run_uplc_validation
}

main "$@"
