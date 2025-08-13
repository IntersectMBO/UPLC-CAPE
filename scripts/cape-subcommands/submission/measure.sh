#!/usr/bin/env bash

# CAPE Submission Measure Script
# Measures UPLC program performance and creates metrics.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"
SUBMISSIONS_ROOT="$PROJECT_ROOT/submissions"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored output
print_error() { echo -e "${RED}ERROR:${NC} $1" >&2; }
print_success() { echo -e "${GREEN}SUCCESS:${NC} $1"; }
print_warning() { echo -e "${YELLOW}WARNING:${NC} $1"; }
print_info() { echo -e "${BLUE}INFO:${NC} $1"; }

# Helper: path relative to project root
relpath() {
  local target="$1"
  if [[ "$target" == "$PROJECT_ROOT" ]]; then
    echo "."
  elif [[ "$target" == "$PROJECT_ROOT"/* ]]; then
    echo "${target#$PROJECT_ROOT/}"
  else
    echo "$target"
  fi
}

# Check if measure tool is available
check_measure_tool() {
  if ! command -v measure &> /dev/null; then
    print_error "measure tool is not available. Please ensure you're running this in the nix development shell."
    print_info "The nix shell includes the measure executable for UPLC performance measurement."
    exit 1
  fi
}

# Measure a UPLC file and create metrics.json
measure_uplc_file() {
  local uplc_file="$1"
  local output_file="$2" # Should be metrics.json path
  local rel_uplc rel_output
  rel_uplc="$(relpath "$uplc_file")"
  rel_output="$(relpath "$output_file")"

  # Initialize metadata holders to avoid unbound errors with nounset
  local existing_notes="" existing_version="" existing_evaluator=""

  if [[ ! -f "$uplc_file" ]]; then
    print_error "UPLC file not found: $rel_uplc"
    return 1
  fi
  if ! command -v jq > /dev/null 2>&1; then
    print_error "jq is required for JSON processing. Please ensure it's available in the development shell."
    return 1
  fi

  local was_existing=0
  [[ -f "$output_file" ]] && was_existing=1

  print_info "Measuring UPLC program: $rel_uplc"
  echo ""

  local tmp_raw
  tmp_raw="$(mktemp)"
  if ! measure -i "$uplc_file" -o "$tmp_raw" | tee /tmp/measure_stdout.$$; then
    print_error "✗ Failed to measure UPLC program ($rel_uplc)"
    rm -f "$tmp_raw" /tmp/measure_stdout.$$
    return 1
  fi

  local stdout_file="/tmp/measure_stdout.$$"
  local evaluator
  evaluator=$(grep -E '^Evaluator:' "$stdout_file" | sed -E 's/^Evaluator: *//') || true

  local submission_dir scenario
  submission_dir="$(dirname "$uplc_file")"
  scenario=""
  case "$submission_dir" in
    *"/submissions/"*)
      scenario="${submission_dir#*'/submissions/'}"
      scenario="${scenario%%/*}"
      ;;
  esac
  [[ -z "$scenario" ]] && scenario="unknown"

  # Preserve existing metadata (notes, version) if metrics.json exists
  if [[ $was_existing -eq 1 ]]; then
    existing_notes=$(jq -r '.notes // empty' "$output_file" 2> /dev/null || echo "") || true
    existing_version=$(jq -r '.version // empty' "$output_file" 2> /dev/null || echo "") || true
    existing_evaluator=$(jq -r '.execution_environment.evaluator // empty' "$output_file" 2> /dev/null || echo "") || true
  fi

  [[ -z "$existing_notes" ]] && existing_notes="Generated using UPLC-CAPE measure tool"
  [[ -z "$existing_version" ]] && existing_version="1.0.0"
  [[ -n "$evaluator" ]] || evaluator="$existing_evaluator"
  [[ -z "$evaluator" ]] && evaluator="unknown"

  local timestamp
  timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

  local tmp_out
  tmp_out="$(mktemp)"
  if ! jq -n \
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
  ' > "$tmp_out"; then
    print_error "Failed to compose metrics.json for $rel_uplc"
    rm -f "$tmp_raw" "$stdout_file" "$tmp_out"
    return 1
  fi

  mv "$tmp_out" "$output_file"
  rm -f "$tmp_raw" "$stdout_file"
  echo ""
  if [[ $was_existing -eq 1 ]]; then
    print_success "Updated $rel_output"
  else
    print_success "Created $rel_output"
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
  rel_dir="$(relpath "$abs_dir")"

  if find "$abs_dir" -name "*.uplc" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      measure_uplc_file "$file" "$(dirname "$file")/metrics.json" || has_errors=1
    done < <(find "$abs_dir" -name "*.uplc" -type f -print0)
  else
    print_warning "No .uplc files found in $rel_dir"
  fi
  return $has_errors
}

# Measure all submissions (explicit when --all provided)
measure_all_submissions() {
  print_info "Measuring all submissions under $(relpath "$SUBMISSIONS_ROOT")"
  local overall_errors=0
  local found_any=0
  for scenario_dir in "$SUBMISSIONS_ROOT"/*/; do
    [[ ! -d "$scenario_dir" ]] && continue
    local scenario_name
    scenario_name="$(basename "$scenario_dir")"
    # Skip TEMPLATE directory
    if [[ "$scenario_name" == "TEMPLATE" ]]; then
      continue
    fi
    for submission_dir in "$scenario_dir"*/; do
      [[ ! -d "$submission_dir" ]] && continue
      found_any=1
      local rel_path
      rel_path="${submission_dir#$SUBMISSIONS_ROOT/}"
      rel_path="${rel_path%/}"
      print_info "Measuring submission: submissions/$rel_path"
      if ! measure_directory "$submission_dir"; then
        overall_errors=1
      fi
    done
  done
  if [[ $found_any -eq 0 ]]; then
    print_warning "No submissions found under $(relpath "$SUBMISSIONS_ROOT")"
  fi
  return $overall_errors
}

# Show usage information
show_usage() {
  cat << EOF
CAPE Submission Measure

USAGE:
    cape submission measure [OPTIONS] ( --all | PATH | -i <file> -o <file> )

DESCRIPTION:
    Measures UPLC program performance including CPU units, memory units,
    script size, and term size. Creates or updates metrics.json files.

    You MUST explicitly choose one of the modes:
      --all                Measure every submission under submissions/
      PATH                 Measure all *.uplc files under a specific path
      -i FILE -o FILE      Measure a single UPLC file to specified metrics.json

OPTIONS:
    -h, --help       Show this help message
    -a, --all        Measure all submissions (cannot be combined with PATH or -i/-o)
    -i, --input      Input UPLC file (requires -o/--output)
    -o, --output     Output metrics.json file (requires -i/--input)

ARGUMENTS:
  PATH            Path to a directory containing .uplc files to measure.
                  Use '.' to measure only the current directory.

EXAMPLES:
    cape submission measure -i fibonacci.uplc -o metrics.json
    cape submission measure --all
    cape submission measure submissions/fibonacci/Plinth_1.49_Unisay
    cape submission measure .

NOTES:
    - Explicit mode selection prevents accidental measurement of all submissions.
    - The measure tool evaluates UPLC programs and extracts performance metrics.
    - Output metrics.json files follow the CAPE framework schema.
    - Requires the nix development shell with the measure tool available.

EOF
}

# Parse command line arguments
input_file=""
output_file=""
target_path=""
measure_all=false

while [[ $# -gt 0 ]]; do
  case $1 in
    -h | --help)
      show_usage
      exit 0
      ;;
    -a | --all)
      measure_all=true
      shift
      ;;
    -i | --input)
      if [[ $# -lt 2 ]]; then
        print_error "Option $1 requires an argument"
        exit 1
      fi
      input_file="$2"
      shift 2
      ;;
    -o | --output)
      if [[ $# -lt 2 ]]; then
        print_error "Option $1 requires an argument"
        exit 1
      fi
      output_file="$2"
      shift 2
      ;;
    -*)
      print_error "Unknown option: $1"
      show_usage
      exit 1
      ;;
    *)
      target_path="$1"
      shift
      ;;
  esac
done

# Enforce explicit mode selection (excluding single-file -i/-o pair which is self-explicit)
if [[ -z "$target_path" && "$measure_all" == false && (-z "$input_file" || -z "$output_file") ]]; then
  print_error "No mode specified."
  echo
  print_info "You must specify one of:"
  echo "  --all                 Measure all submissions under submissions/"
  echo "  <path>                Measure all .uplc files under the given path"
  echo "  -i <file> -o <file>   Measure a single file into metrics.json"
  echo
  print_info "Examples:"
  echo "  cape submission measure --all"
  echo "  cape submission measure submissions/fibonacci/Plinth_1.49_Unisay"
  echo "  cape submission measure -i fibonacci.uplc -o metrics.json"
  echo
  print_info "Run with --help to see full usage."
  exit 1
fi

# Prevent conflicting modes
if [[ "$measure_all" == true && -n "$target_path" ]]; then
  print_error "Cannot use --all together with a PATH argument"
  exit 1
fi
if [[ "$measure_all" == true && (-n "$input_file" || -n "$output_file") ]]; then
  print_error "Cannot combine --all with -i/--input or -o/--output"
  exit 1
fi
if [[ -n "$target_path" && (-n "$input_file" || -n "$output_file") ]]; then
  print_error "Cannot combine PATH argument with -i/--input or -o/--output"
  exit 1
fi

# Main measurement logic
main() {
  check_measure_tool

  local exit_code=0

  # If both input and output are specified, measure single file
  if [[ -n "$input_file" && -n "$output_file" ]]; then
    measure_uplc_file "$input_file" "$output_file"
    exit_code=$?

  elif [[ -n "$input_file" || -n "$output_file" ]]; then
    print_error "Both -i/--input and -o/--output are required when measuring a single file"
    show_usage
    exit 1

  else
    if [[ "$measure_all" == true ]]; then
      measure_all_submissions
      exit_code=$?
    else
      local dir_to_measure="$target_path"
      if [[ ! -d "$dir_to_measure" ]]; then
        print_error "Directory not found: $dir_to_measure"
        exit 1
      fi
      measure_directory "$dir_to_measure"
      exit_code=$?
    fi
  fi

  if [[ $exit_code -eq 0 ]]; then
    print_success "All measurements completed successfully!"
  else
    print_error "Some measurements failed. Please check the output above."
  fi

  exit $exit_code
}

main "$@"
