#!/usr/bin/env bash

# CAPE Submission Measure Script
# Measures UPLC program performance and creates metrics.json

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

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
  local output_file="$2"

  if [[ ! -f "$uplc_file" ]]; then
    print_error "UPLC file not found: $uplc_file"
    return 1
  fi

  print_info "Measuring UPLC program: $(basename "$uplc_file")"

  if measure -i "$uplc_file" -o "$output_file"; then
    print_success "✓ Metrics written to $(basename "$output_file")"
    return 0
  else
    print_error "✗ Failed to measure UPLC program"
    return 1
  fi
}

# Find and measure all UPLC files in a directory
measure_directory() {
  local dir="$1"
  local has_errors=0

  print_info "Measuring UPLC programs in directory: $dir"

  # Find and measure .uplc files
  if find "$dir" -name "*.uplc" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      # Create metrics.json in the same directory as the .uplc file
      local dir_name=$(dirname "$file")
      local output_file="$dir_name/metrics.json"

      if ! measure_uplc_file "$file" "$output_file"; then
        has_errors=1
      fi
    done < <(find "$dir" -name "*.uplc" -type f -print0)
  else
    print_warning "No .uplc files found in $dir"
  fi

  return $has_errors
}

# Show usage information
show_usage() {
  cat << EOF
CAPE Submission Measure

USAGE:
    cape submission measure [OPTIONS] [PATH]

DESCRIPTION:
    Measures UPLC program performance including CPU units, memory units,
    script size, and term size. Creates or updates metrics.json files.

OPTIONS:
    -h, --help       Show this help message
    -i, --input      Input UPLC file (requires -o/--output)
    -o, --output     Output metrics.json file (requires -i/--input)

ARGUMENTS:
  PATH            Path to a directory containing .uplc files to measure

EXAMPLES:
    # Measure a specific UPLC file
    cape submission measure -i fibonacci.uplc -o metrics.json

    # Measure all UPLC files in a submission directory
    cape submission measure submissions/fibonacci

    # Measure all UPLC files in current directory
    cape submission measure .

NOTES:
    - The measure tool evaluates UPLC programs and extracts performance metrics
    - Output metrics.json files follow the CAPE framework schema
    - Requires the nix development shell with the measure tool available

EOF
}

# Parse command line arguments
input_file=""
output_file=""
target_path=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -h | --help)
      show_usage
      exit 0
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
    # Measure directory (current directory if no path specified)
    local dir_to_measure="${target_path:-$PWD}"

    if [[ ! -d "$dir_to_measure" ]]; then
      print_error "Directory not found: $dir_to_measure"
      exit 1
    fi

    measure_directory "$dir_to_measure"
    exit_code=$?
  fi

  if [[ $exit_code -eq 0 ]]; then
    print_success "All measurements completed successfully!"
  else
    print_error "Some measurements failed. Please check the output above."
  fi

  exit $exit_code
}

main "$@"
