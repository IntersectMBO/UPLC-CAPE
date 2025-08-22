#!/usr/bin/env bash

# CAPE Submission Verify Script
# Performs correctness verification (via measure) and schema validation
# Requires cape-tests.json for all scenarios - no simple measurement mode

set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"
SUBMISSIONS_ROOT="$PROJECT_ROOT/submissions"
SCENARIOS_ROOT="$PROJECT_ROOT/scenarios"
SCHEMAS_DIR="$PROJECT_ROOT/submissions/TEMPLATE"

# Enable shared tmp cleanup trap
cape_enable_tmp_cleanup

# Verbosity
VERBOSE=0
if [[ -n "${CAPE_VERBOSE:-}" ]]; then VERBOSE=1; fi

if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

cape_require_cmd jq
cape_require_cmd check-jsonschema
cape_require_cmd cabal

infer_scenario_from_path() {
  local path="$1"
  case "$path" in
    *"/submissions/"* | "submissions/"*)
      local tail
      if [[ "$path" == "submissions/"* ]]; then
        tail="${path#'submissions/'}"
      else
        tail="${path#*'/submissions/'}"
      fi
      echo "${tail%%/*}"
      ;;
    *)
      echo ""
      ;;
  esac
}

resolve_tests_for_scenario() {
  local scen="$1"
  local preferred="$SCENARIOS_ROOT/$scen/cape-tests.json"
  if [[ -f "$preferred" ]]; then
    echo "$preferred"
  else
    echo ""
  fi
}

verify_submission_dir() {
  local submission_dir="$1"
  local rel_dir
  rel_dir="$(cape_relpath "$submission_dir")"

  cape_info "Verifying submission: $rel_dir"

  # Ensure expected files exist
  local uplc_file="$submission_dir/${submission_dir##*/}.uplc"
  if [[ ! -f "$uplc_file" ]]; then
    # Fallback: any .uplc file
    uplc_file="$(find "$submission_dir" -maxdepth 1 -type f -name '*.uplc' | head -n1 || true)"
  fi
  if [[ -z "$uplc_file" ]]; then
    cape_error "No .uplc file found in $rel_dir"
    return 1
  fi

  local scenario
  scenario="$(infer_scenario_from_path "$submission_dir")"

  local tests_flag=()

  # Prefer a submission-local cape-tests.json when available
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
        return 1
      fi
    else
      cape_error "Unable to infer scenario for $rel_dir. Test specification is required."
      cape_error "Ensure submission follows the correct directory structure."
      return 1
    fi
  fi

  local tmp_metrics tmp_stdout
  tmp_metrics="$(cape_mktemp)"
  tmp_stdout="$(cape_mktemp)"

  local measure_cmd
  measure_cmd="$(cape_measure_binary)"

  local measure_rc
  if [[ $VERBOSE -eq 1 ]]; then
    if (cd "$PROJECT_ROOT" && $measure_cmd -i "$uplc_file" "${tests_flag[@]}" -o "$tmp_metrics" 2> /dev/null) | tee "$tmp_stdout"; then
      measure_rc=0
    else
      measure_rc=1
    fi
  else
    if (cd "$PROJECT_ROOT" && $measure_cmd -i "$uplc_file" "${tests_flag[@]}" -o "$tmp_metrics" 2> /dev/null) > "$tmp_stdout"; then
      measure_rc=0
    else
      measure_rc=1
    fi
  fi

  case "$measure_rc" in
    0)
      cape_success "✓ UPLC code evaluates as expected."
      # Only update metrics.json when measurement succeeds
      ;;
    1)
      # Display detailed test failure information
      if [[ -f "$tmp_stdout" && -s "$tmp_stdout" ]]; then
        echo
        cat "$tmp_stdout"
        echo
      fi
      cape_error "✗ UPLC code evaluation failed."
      return 1
      ;;
    2)
      # Display detailed parse error information
      if [[ -f "$tmp_stdout" && -s "$tmp_stdout" ]]; then
        echo
        cat "$tmp_stdout"
        echo
      fi
      cape_error "UPLC parse error or malformed input"
      return 1
      ;;
    *)
      # Display any available output for unexpected errors
      if [[ -f "$tmp_stdout" && -s "$tmp_stdout" ]]; then
        echo
        cat "$tmp_stdout"
        echo
      fi
      cape_error "✗ UPLC code evaluation failed with unexpected error."
      return 1
      ;;
  esac

  # Write/merge metrics.json - only reached when measure_rc=0
  local metrics_out="$submission_dir/metrics.json"
  local evaluator
  evaluator=$(grep -E '^Evaluator:' "$tmp_stdout" | sed -E 's/^Evaluator: *//') || true
  local timestamp
  timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  local existing_notes existing_version existing_evaluator
  existing_notes=""
  existing_version=""
  existing_evaluator=""
  if [[ -f "$metrics_out" ]]; then
    existing_notes=$(jq -r '.notes // empty' "$metrics_out" 2> /dev/null || echo "") || true
    existing_version=$(jq -r '.version // empty' "$metrics_out" 2> /dev/null || echo "") || true
    existing_evaluator=$(jq -r '.execution_environment.evaluator // empty' "$metrics_out" 2> /dev/null || echo "") || true
  fi
  if [[ -z "$existing_notes" ]]; then existing_notes="Generated using UPLC-CAPE measure tool"; fi
  if [[ -z "$existing_version" ]]; then existing_version="1.0.0"; fi
  if [[ -z "$evaluator" ]]; then evaluator="$existing_evaluator"; fi
  if [[ -z "$evaluator" ]]; then evaluator="unknown"; fi

  local tmp_out
  tmp_out="$(cape_mktemp)"
  if ! jq -n \
    --slurpfile m "$tmp_metrics" \
    --arg evaluator "$evaluator" \
    --arg notes "$existing_notes" \
    --arg scenario "${scenario:-unknown}" \
    --arg version "$existing_version" \
    --arg timestamp "$timestamp" '
    $m[0] + {
      execution_environment: { evaluator: $evaluator },
      notes: $notes,
      scenario: $scenario,
      timestamp: $timestamp,
      version: $version
    }
  ' > "$tmp_out"; then
    cape_error "Failed to compose metrics.json for $rel_dir"
    rm -f "$tmp_metrics" "$tmp_stdout" "$tmp_out" || true
    return 1
  fi
  mv "$tmp_out" "$metrics_out"
  rm -f "$tmp_metrics" "$tmp_stdout" || true

  # Validate JSON files against schemas
  local schema_metrics="$SCHEMAS_DIR/metrics.schema.json"
  local schema_metadata="$SCHEMAS_DIR/metadata.schema.json"

  local ok=1
  local result
  result=$(check-jsonschema --output-format json --schemafile "$schema_metrics" "$metrics_out" 2> /dev/null)
  local status
  status=$(echo "$result" | jq -r '.status')

  if [[ "$status" == "ok" ]]; then
    cape_success "✓ metrics.json conforms to $(basename "$schema_metrics")"
  else
    cape_error "✗ metrics.json does not conform to $(basename "$schema_metrics")"
    ok=0
  fi

  local metadata_file="$submission_dir/metadata.json"
  if [[ ! -f "$metadata_file" ]]; then
    cape_error "metadata.json not found in $rel_dir"
    ok=0
  else
    local metadata_result
    metadata_result=$(check-jsonschema --output-format json --schemafile "$schema_metadata" "$metadata_file" 2> /dev/null)
    local metadata_status
    metadata_status=$(echo "$metadata_result" | jq -r '.status')

    if [[ "$metadata_status" == "ok" ]]; then
      cape_success "✓ metadata.json conforms to $(basename "$schema_metadata")"
    else
      cape_error "✗ metadata.json does not conform to $(basename "$schema_metadata")"
      ok=0
    fi
  fi

  if [[ $ok -ne 1 ]]; then
    return 1
  fi

  cape_success "Verification complete for $rel_dir"
  return 0
}

# CLI parsing: supports --all or a positional PATH.
TARGET=""
VERIFY_ALL=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -a | --all)
      VERIFY_ALL=true
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
      # Positional PATH
      TARGET="$1"
      shift
      ;;
  esac

done

# Validate mode selection
if [[ "$VERIFY_ALL" == true && -n "$TARGET" ]]; then
  cape_error "Cannot use --all together with a PATH argument"
  exit 1
fi

if [[ "$VERIFY_ALL" == false && -z "$TARGET" ]]; then
  cape_error "No target specified. Provide a PATH or use --all."
  cape_render_help "${BASH_SOURCE[0]}"
  exit 1
fi

main() {
  local rc=0
  if [[ "$VERIFY_ALL" == true ]]; then
    cape_info "Verifying all submissions under $(cape_relpath "$SUBMISSIONS_ROOT")"
    local any_found=0
    while IFS= read -r submission_dir; do
      any_found=1
      if ! verify_submission_dir "$submission_dir"; then
        rc=1
      fi
    done < <(cape_each_submission_dir "$SUBMISSIONS_ROOT")
    if [[ $any_found -eq 0 ]]; then
      cape_warn "No submissions found under $(cape_relpath "$SUBMISSIONS_ROOT")"
    fi
  else
    if [[ ! -d "$TARGET" ]]; then
      cape_error "Directory not found: $TARGET"
      exit 1
    fi
    verify_submission_dir "$TARGET" || rc=$?
  fi

  if [[ $rc -eq 0 ]]; then
    cape_success "All verifications completed successfully!"
  else
    cape_error "Some verifications failed. Please check the output above."
  fi
  exit $rc
}

main "$@"
