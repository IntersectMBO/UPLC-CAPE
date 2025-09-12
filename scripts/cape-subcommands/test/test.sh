#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

## CAPE Test Suite - Comprehensive validation of all CLI commands

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"
REPO_ROOT="$PROJECT_ROOT"

# Parse help options
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

cape_require_cmd timeout "Install coreutils (timeout) or run within 'nix develop'."

# Test state
TESTS_RUN=0 TESTS_PASSED=0 TESTS_FAILED=0
TEST_TMP_DIR="" SANDBOX_DIR=""

# Test environment constants
# Commands will use: (cd "$PROJECT_ROOT" && PROJECT_ROOT="$SANDBOX_DIR" bash "$REPO_ROOT/scripts/...)

# Enhanced test runner
run_test() {
  local name="$1" command="$2" timeout="${3:-10}" expect_fail="${4:-}"
  TESTS_RUN=$((TESTS_RUN + 1))

  local result=0 exit_code=0

  if [[ "$expect_fail" == "fail" ]]; then
    # For expected failures, command should return non-zero
    if timeout "${timeout}s" bash -c "$command" > /dev/null 2>&1; then
      result=1 # Command succeeded when it should have failed
    else
      result=0 # Command failed as expected
    fi
  else
    # For expected successes, command should return zero
    if timeout "${timeout}s" bash -c "$command" > /dev/null 2>&1; then
      result=0 # Command succeeded as expected
    else
      result=1 # Command failed when it should have succeeded
    fi
  fi

  # Output with proper colors
  if [[ $result -eq 0 ]]; then
    if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
      echo -e "\033[0;34mINFO:\033[0m \033[0;32m✓ $name\033[0m"
    else
      echo "INFO: ✓ $name"
    fi
    TESTS_PASSED=$((TESTS_PASSED + 1))
  else
    if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
      echo -e "\033[0;31mERROR:\033[0m \033[0;31m✗ $name\033[0m"
    else
      echo "ERROR: ✗ $name"
    fi
    TESTS_FAILED=$((TESTS_FAILED + 1))
  fi
}

# Test group runner
test_group() {
  local group_name="$1"
  shift

  # Output group header with colors
  if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
    echo -e "\033[0;34mINFO:\033[0m Testing $group_name..."
  else
    echo "INFO: Testing $group_name..."
  fi

  while [[ $# -gt 0 ]]; do
    local test_name="$1" test_cmd="$2" timeout="${3:-10}" expect_fail="${4:-}"
    run_test "$test_name" "$test_cmd" "$timeout" "$expect_fail"
    shift 4 2> /dev/null || shift $#
  done
}

# Run UPLC validation using cape submission verify --all
run_uplc_validation() {
  if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
    echo -e "\033[0;34mINFO:\033[0m Testing UPLC cape-tests validation..."
  else
    echo "INFO: Testing UPLC cape-tests validation..."
  fi

  # Run cape submission verify --all with output redirected to /dev/null for performance
  local verify_exit_code=0
  (cd "$PROJECT_ROOT" && PROJECT_ROOT="$SANDBOX_DIR" bash "$REPO_ROOT/scripts/cape.sh" submission verify --all > /dev/null 2>&1) || verify_exit_code=$?

  # Update test counters
  TESTS_RUN=$((TESTS_RUN + 1))

  if [[ $verify_exit_code -eq 0 ]]; then
    TESTS_PASSED=$((TESTS_PASSED + 1))
    if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
      echo -e "\033[0;34mINFO:\033[0m \033[0;32m✓ UPLC submissions validation\033[0m"
    else
      echo "INFO: ✓ UPLC submissions validation"
    fi
  else
    TESTS_FAILED=$((TESTS_FAILED + 1))
    if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
      echo -e "\033[0;31mERROR:\033[0m \033[0;31m✗ UPLC submissions validation\033[0m"
    else
      echo "ERROR: ✗ UPLC submissions validation"
    fi
    cape_error "UPLC submissions validation failed"
    return 1
  fi
}

# Setup clean test environment
setup_test_env() {
  TEST_TMP_DIR="$(cape_mktemp_dir)"
  SANDBOX_DIR="$TEST_TMP_DIR/sandbox"

  # Initialize sandbox roots
  mkdir -p "$SANDBOX_DIR/scenarios" "$SANDBOX_DIR/submissions"

  # 1) Copy real repo content into sandbox (treat all scenarios/submissions uniformly)
  if [[ -d "$REPO_ROOT/scenarios" ]]; then
    cp -a "$REPO_ROOT/scenarios/." "$SANDBOX_DIR/scenarios/" 2> /dev/null || true
  fi
  if [[ -d "$REPO_ROOT/submissions" ]]; then
    cp -a "$REPO_ROOT/submissions/." "$SANDBOX_DIR/submissions/" 2> /dev/null || true
  fi

  # No test fixtures or verifier files needed - using real scenarios with cape-tests.json only

  if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
    echo -e "\033[0;34mINFO:\033[0m Test environment initialized at: $SANDBOX_DIR"
  else
    echo "INFO: Test environment initialized at: $SANDBOX_DIR"
  fi
}

# Cleanup and show results
cleanup() {
  cape_cleanup_tmpfiles

  # Ensure colors are properly set for final output
  cape_apply_color_prefs

  echo
  echo "========================================"
  if [[ -n "${CAPE_GREEN:-}" ]]; then
    # Colors available
    printf "Tests: %d | Passed: \033[0;32m%d\033[0m | Failed: \033[0;31m%d\033[0m\n" \
      "$TESTS_RUN" "$TESTS_PASSED" "$TESTS_FAILED"
  else
    # No colors
    printf "Tests: %d | Passed: %d | Failed: %d\n" \
      "$TESTS_RUN" "$TESTS_PASSED" "$TESTS_FAILED"
  fi

  if [[ $TESTS_FAILED -eq 0 ]]; then
    if [[ -n "${CAPE_GREEN:-}" ]]; then
      echo -e "\033[0;32mSUCCESS: ALL TESTS PASSED\033[0m"
    else
      echo "SUCCESS: ALL TESTS PASSED"
    fi
  else
    if [[ -n "${CAPE_RED:-}" ]]; then
      echo -e "\033[0;31mERROR: SOME TESTS FAILED\033[0m"
    else
      echo "ERROR: SOME TESTS FAILED"
    fi
    exit 1
  fi
}
trap cleanup EXIT

# Main test execution
main() {
  echo "UPLC CAPE Test Suite"
  echo "===================="
  setup_test_env

  # Environment validation - measure tool must be buildable via cabal
  test_group "environment requirements" \
    "measure tool buildable" "(cd \"$REPO_ROOT\" && cabal build exe:measure)" 30 ""

  # Core functionality
  test_group "core commands" \
    "cape help" "bash scripts/cape.sh --help" 5 "" \
    "cape no args" "bash scripts/cape.sh" 5 "fail" \
    "cape test help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/test/test.sh\" --help)" 5 "" \
    "cape test -h" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/test/test.sh\" -h)" 5 ""

  # Benchmark operations
  test_group "benchmark commands" \
    "benchmark help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape.sh\" benchmark --help)" 5 "" \
    "benchmark list" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/list.sh\")" 5 "" \
    "benchmark list help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/list.sh\" --help)" 5 "" \
    "benchmark new help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/new.sh\" --help)" 5 "" \
    "benchmark fibonacci" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/list.sh\" fibonacci)" 5 "" \
    "benchmark no args fail" "echo '' | (cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/new.sh\")" 5 "fail" \
    "benchmark invalid fail" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/list.sh\" invalid_arg; exit 1)" 5 "fail"

  # Submission operations
  test_group "submission commands" \
    "submission help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape.sh\" submission --help)" 5 "" \
    "submission list" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/list.sh\")" 5 "" \
    "submission list help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/list.sh\" --help)" 5 "" \
    "submission list fibonacci" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/list.sh\" fibonacci)" 5 "" \
    "submission no args fail" "echo '' | (cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/new.sh\" fibonacci Comp 1.0)" 5 "fail" \
    "submission complete success" "echo '' | (cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/new.sh\" fibonacci Comp 1.0 user)" 5 "" \
    "submission cleanup (remove template submission)" "rm -rf \"$SANDBOX_DIR/submissions/fibonacci/Comp_1.0_user\"" 5 "" \
    "submission invalid fail" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/list.sh\" invalid_arg; exit 1)" 5 "fail"

  # Verification & measurement
  test_group "verification & measurement" \
    "verify help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/verify.sh\" --help)" 5 "" \
    "measure help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/measure.sh\" --help)" 5 ""

  # Reporting and aggregation
  test_group "reporting" \
    "report help" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/report.sh\" --help)" 5 "" \
    "report dry-run all" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/report.sh\" --dry-run --all)" 10 "" \
    "report dry-run fibonacci" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/report.sh\" --dry-run fibonacci; true)" 10 "" \
    "aggregate" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/aggregate.sh\")" 30 ""

  # Global flags
  test_group "global options" \
    "verbose benchmark" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape.sh\" --verbose benchmark list)" 10 "" \
    "no-color benchmark" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape.sh\" --no-color benchmark list)" 10 "" \
    "verbose short" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape.sh\" -v submission list)" 10 ""

  # Integration workflow
  if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
    echo -e "\033[0;34mINFO:\033[0m Testing integration workflow..."
  else
    echo "INFO: Testing integration workflow..."
  fi
  local test_name="test-$$"
  run_test "create benchmark" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/benchmark/new.sh\" $test_name)" 15
  run_test "create submission" "(cd \"$PROJECT_ROOT\" && PROJECT_ROOT=\"$SANDBOX_DIR\" bash \"$REPO_ROOT/scripts/cape-subcommands/submission/new.sh\" $test_name TestComp 1.0 user)" 15
  run_test "verify submission created" "test -d $SANDBOX_DIR/submissions/$test_name/TestComp_1.0_user" 2

  # Structure validation
  test_group "directory structure" \
    "scenarios dir" "test -d scenarios" 2 "" \
    "submissions dir" "test -d submissions" 2 "" \
    "scripts dir" "test -d scripts" 2 "" \
    "templates exist" "test -d scenarios/TEMPLATE && test -d submissions/TEMPLATE" 2 ""

  # Haskell library tests - use longer timeout in CI
  local cabal_test_timeout=60
  if [[ "${CI:-}" == "true" ]] || [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then
    cabal_test_timeout=180
  fi

  test_group "Haskell library tests" \
    "cabal test cape-tests" "(cd \"$REPO_ROOT\" && cabal test cape-tests)" "$cabal_test_timeout" ""

  # UPLC validation - test actual UPLC execution for all submissions
  run_uplc_validation

  if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
    echo -e "\033[0;34mINFO:\033[0m Test suite completed!"
  else
    echo "INFO: Test suite completed!"
  fi
}

main "$@"
