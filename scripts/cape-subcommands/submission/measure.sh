#!/usr/bin/env bash

# CAPE Submission Measure Script
# Measures UPLC program performance and creates metrics.json

set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"
SUBMISSIONS_ROOT="$PROJECT_ROOT/submissions"
SCENARIOS_ROOT="$PROJECT_ROOT/scenarios"
# Enable shared tmp cleanup trap
cape_enable_tmp_cleanup

# Verbosity
VERBOSE=0
# Inherit from global flag exported by cape.sh
if [[ -n "${CAPE_VERBOSE:-}" ]]; then VERBOSE=1; fi

# Use shared help
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Check if measure tool can be built
check_measure_tool() {
  if ! command -v cabal > /dev/null 2>&1; then
    cape_error "cabal is required to build the measure tool"
    return 1
  fi
  if [[ "${VERBOSE:-0}" -eq 1 ]]; then
    cape_info "Building measure tool..."
    (cd "$PROJECT_ROOT/measure" && cabal build exe:measure) || cape_error "Failed to build measure tool"
  fi
}

# Derive scenario name from a path under submissions/*
# Echoes scenario name or empty string if unknown
infer_scenario_from_path() {
  local path="$1"
  case "$path" in
    *"/submissions/"*)
      local tail
      tail="${path#*'/submissions/'}"
      echo "${tail%%/*}"
      ;;
    *)
      echo ""
      ;;
  esac
}

# Resolve cape-tests.json path for a scenario
resolve_tests_for_scenario() {
  local scen="$1"
  local preferred="$SCENARIOS_ROOT/$scen/cape-tests.json"
  if [[ -f "$preferred" ]]; then
    echo "$preferred"
  else
    echo ""
  fi
}

# Measure a UPLC file and create metrics.json
measure_uplc_file() {
  local uplc_file="$1"
  local output_file="$2" # Should be metrics.json path
  local rel_uplc rel_output
  rel_uplc="$(cape_relpath "$uplc_file")"
  rel_output="$(cape_relpath "$output_file")"

  # Initialize metadata holders to avoid unbound errors with nounset
  local existing_notes="" existing_version="" existing_evaluator=""

  if [[ ! -f "$uplc_file" ]]; then
    cape_error "UPLC file not found: $rel_uplc"
    return 1
  fi
  cape_require_cmd jq

  local was_existing=0
  if [[ -f "$output_file" ]]; then
    was_existing=1
  fi

  cape_info "Measuring UPLC program: $rel_uplc"
  echo ""

  # Auto-determine cape-tests.json to pass to measure (-t <file>)
  local scenario
  scenario="$(infer_scenario_from_path "$uplc_file")"

  local tests_flag=()
  # Prefer a submission-local cape-tests.json when available
  local submission_dir
  submission_dir="$(dirname "$uplc_file")"
  local submission_tests="$submission_dir/cape-tests.json"
  if [[ -f "$submission_tests" ]]; then
    cape_debug "Using submission tests: $(cape_relpath "$submission_tests")"
    tests_flag+=("-t" "$submission_tests")
  else
    if [[ -n "$scenario" ]]; then
      local tests_file
      tests_file="$(resolve_tests_for_scenario "$scenario")"
      if [[ -n "$tests_file" ]]; then
        cape_debug "Using scenario tests: $(cape_relpath "$tests_file")"
        tests_flag+=("-t" "$tests_file")
      else
        cape_error "No cape-tests.json found for scenario '$scenario'. Test specification is required."
        cape_error "Create scenarios/$scenario/cape-tests.json with test cases."
        exit 1
      fi
    else
      cape_error "Unable to infer scenario for $rel_uplc. Test specification is required."
      cape_error "Ensure the UPLC file is in a proper submission directory structure."
      exit 1
    fi
  fi

  local tmp_raw stdout_tmp
  tmp_raw="$(cape_mktemp)"
  stdout_tmp="$(cape_mktemp)"

  if [[ $VERBOSE -eq 1 ]]; then
    if ! (cd "$PROJECT_ROOT/measure" && cabal run measure -- -i "$uplc_file" "${tests_flag[@]}" -o "$tmp_raw") 2>&1 | tee "$stdout_tmp"; then
      cape_error "✗ Failed to measure UPLC program ($rel_uplc)"
      echo ""
      cape_error "Test execution details:"
      cat "$stdout_tmp" | grep -E "Running test:|✓ PASS|✗ FAIL|Error:" | sed 's/^/  /'
      rm -f "$tmp_raw" "$stdout_tmp" || true
      return 1
    fi
  else
    if ! (cd "$PROJECT_ROOT/measure" && cabal run measure -- -i "$uplc_file" "${tests_flag[@]}" -o "$tmp_raw") > "$stdout_tmp" 2>&1; then
      cape_error "✗ Failed to measure UPLC program ($rel_uplc)"
      echo ""
      cape_error "Test execution details:"
      cat "$stdout_tmp" | grep -E "Running test:|✓ PASS|✗ FAIL|Error:|Test results:" | sed 's/^/  /'
      echo ""
      cape_error "Hint: Check test data format if you see parse errors. BuiltinData uses:"
      cape_error "  - Integers: 42, -123"
      cape_error "  - ByteStrings: #deadbeef"
      cape_error "  - Constructors: 0(), 1(42 #cafe)"
      cape_error "  - Lists: [1 2 3]"
      cape_error "  - Maps: {1:42}"
      rm -f "$tmp_raw" "$stdout_tmp" || true
      return 1
    fi
  fi

  local stdout_file
  stdout_file="$stdout_tmp"
  local evaluator
  evaluator=$(grep -E '^Evaluator:' "$stdout_file" | sed -E 's/^Evaluator: *//') || true
  if [[ -n "$evaluator" ]]; then
    cape_debug "Evaluator detected: $evaluator"
  fi

  # Preserve existing metadata (notes, version) if metrics.json exists
  local submission_dir_out
  submission_dir_out="$(dirname "$uplc_file")"
  if [[ -z "$scenario" ]]; then
    scenario="unknown"
  fi

  if [[ $was_existing -eq 1 ]]; then
    existing_notes=$(jq -r '.notes // empty' "$output_file" 2> /dev/null || echo "") || true
    existing_version=$(jq -r '.version // empty' "$output_file" 2> /dev/null || echo "") || true
    existing_evaluator=$(jq -r '.execution_environment.evaluator // empty' "$output_file" 2> /dev/null || echo "") || true
  fi

  if [[ -z "$existing_notes" ]]; then
    existing_notes="Generated using UPLC-CAPE measure tool"
  fi
  if [[ -z "$existing_version" ]]; then
    existing_version="1.0.0"
  fi
  if [[ -z "$evaluator" ]]; then
    evaluator="$existing_evaluator"
  fi
  if [[ -z "$evaluator" ]]; then
    evaluator="unknown"
  fi

  local timestamp
  timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

  # Preserve existing timestamp if measurements are identical
  if [[ $was_existing -eq 1 ]]; then
    # Create temporary metrics with new measurements for comparison
    local tmp_new_metrics
    tmp_new_metrics="$(cape_mktemp)"
    if jq -n \
      --slurpfile m "$tmp_raw" \
      --arg evaluator "$evaluator" \
      --arg notes "$existing_notes" \
      --arg scenario "$scenario" \
      --arg version "$existing_version" \
      --arg timestamp "$timestamp" '
      {
        execution_environment: {
          evaluator: $evaluator
        },
        measurements: $m[0],
        notes: $notes,
        scenario: $scenario,
        timestamp: $timestamp,
        version: $version
      }
    ' > "$tmp_new_metrics" 2> /dev/null; then

      # Compare new metrics with existing (excluding timestamp)
      local new_metrics_no_ts existing_metrics_no_ts
      new_metrics_no_ts=$(jq 'del(.timestamp)' "$tmp_new_metrics" 2> /dev/null || echo "{}")
      existing_metrics_no_ts=$(jq 'del(.timestamp)' "$output_file" 2> /dev/null || echo "{}")

      if [[ "$new_metrics_no_ts" == "$existing_metrics_no_ts" ]]; then
        # Preserve original timestamp if measurements are identical
        local existing_timestamp
        existing_timestamp=$(jq -r '.timestamp // empty' "$output_file" 2> /dev/null || echo "")
        if [[ -n "$existing_timestamp" ]]; then
          timestamp="$existing_timestamp"
          cape_debug "Measurements unchanged, preserving timestamp: $existing_timestamp"
        fi
      fi
    fi
    rm -f "$tmp_new_metrics" || true
  fi

  local tmp_out
  tmp_out="$(cape_mktemp)"
  if ! jq -n \
    --slurpfile m "$tmp_raw" \
    --arg evaluator "$evaluator" \
    --arg notes "$existing_notes" \
    --arg scenario "$scenario" \
    --arg version "$existing_version" \
    --arg timestamp "$timestamp" '
    $m[0] + {
      execution_environment: {
        evaluator: $evaluator
      },
      notes: $notes,
      scenario: $scenario,
      timestamp: $timestamp,
      version: $version
    }
  ' > "$tmp_out"; then
    cape_error "Failed to compose metrics.json for $rel_uplc"
    rm -f "$tmp_raw" "$stdout_file" "$tmp_out" || true
    return 1
  fi

  mv "$tmp_out" "$output_file"
  rm -f "$tmp_raw" "$stdout_file" || true
  echo ""
  if [[ $was_existing -eq 1 ]]; then
    cape_success "Updated $rel_output"
  else
    cape_success "Created $rel_output"
  fi
  echo ""
  return 0
}

# Find and measure all UPLC files in a directory
measure_directory() {
  local dir="$1"
  local has_errors=0
  local abs_dir
  abs_dir="$(cd "$dir" && pwd)" || return 1
  local rel_dir
  rel_dir="$(cape_relpath "$abs_dir")"

  if find "$abs_dir" -name "*.uplc" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      cape_debug "Measuring file: $(cape_relpath "$file")"
      measure_uplc_file "$file" "$(dirname "$file")/metrics.json" || has_errors=1
    done < <(find "$abs_dir" -name "*.uplc" -type f -print0)
  else
    cape_warn "No .uplc files found in $rel_dir"
  fi
  return $has_errors
}

# Measure all submissions (explicit when --all provided)
measure_all_submissions() {
  cape_info "Measuring all submissions under $(cape_relpath "$SUBMISSIONS_ROOT")"
  local overall_errors=0
  local found_any=0
  local submission_dir
  while IFS= read -r submission_dir; do
    [[ -d "$submission_dir" ]] || continue
    found_any=1
    local rel_path
    rel_path="${submission_dir#$SUBMISSIONS_ROOT/}"
    cape_info "Measuring submission: submissions/${rel_path}"
    if ! measure_directory "$submission_dir"; then
      overall_errors=1
    fi
  done < <(cape_each_submission_dir "$SUBMISSIONS_ROOT")
  if [[ $found_any -eq 0 ]]; then
    cape_warn "No submissions found under $(cape_relpath "$SUBMISSIONS_ROOT")"
  fi
  return $overall_errors
}

# Parse command line arguments
target_path=""
measure_all=false

while [[ $# -gt 0 ]]; do
  case $1 in
    -a | --all)
      measure_all=true
      shift
      ;;
    --verbose)
      VERBOSE=1
      export CAPE_VERBOSE=1
      shift
      ;;
    --no-color)
      NO_COLOR=1
      cape_apply_color_prefs
      shift
      ;;
    -*)
      cape_error "Unknown option: $1"
      cape_render_help "${BASH_SOURCE[0]}"
      exit 1
      ;;
    *)
      target_path="$1"
      shift
      ;;
  esac
done

# Enforce explicit mode selection
if [[ -z "$target_path" && "$measure_all" == false ]]; then
  cape_error "No mode specified. Provide a PATH or use --all"
  cape_render_help "${BASH_SOURCE[0]}"
  exit 1
fi

# Prevent conflicting modes
if [[ "$measure_all" == true && -n "$target_path" ]]; then
  cape_error "Cannot use --all together with a PATH argument"
  exit 1
fi

# Main measurement logic
main() {
  check_measure_tool

  local exit_code=0

  if [[ "$measure_all" == true ]]; then
    measure_all_submissions
    exit_code=$?
  else
    local dir_to_measure="$target_path"
    if [[ ! -d "$dir_to_measure" ]]; then
      cape_error "Directory not found: $dir_to_measure"
      exit 1
    fi
    measure_directory "$dir_to_measure"
    exit_code=$?
  fi

  if [[ $exit_code -eq 0 ]]; then
    cape_success "All measurements completed successfully!"
  else
    cape_error "Some measurements failed. Please check the output above."
  fi

  exit $exit_code
}

main "$@"
