#!/usr/bin/env bash

# CAPE Submission Validation Script
# Validates submission files against JSON schemas

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

# Schema file paths
METRICS_SCHEMA="$PROJECT_ROOT/submissions/TEMPLATE/metrics.schema.json"
METADATA_SCHEMA="$PROJECT_ROOT/submissions/TEMPLATE/metadata.schema.json"

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

# Check if check-jsonschema is available
check_jsonschema() {
  if ! command -v check-jsonschema &> /dev/null; then
    print_error "check-jsonschema is not available. Please ensure you're running this in the nix development shell."
    print_info "The nix shell includes python3Packages.check-jsonschema for JSON schema validation."
    exit 1
  fi
}

# Validate a single file against a schema
validate_file() {
  local file="$1"
  local schema="$2"
  local file_type="$3"

  if [[ ! -f "$file" ]]; then
    print_warning "File not found: $file"
    return 1
  fi

  if [[ ! -f "$schema" ]]; then
    print_error "Schema not found: $schema"
    return 1
  fi

  print_info "Validating $file_type: $(basename "$file")"

  if check-jsonschema --schemafile "$schema" "$file" 2> /dev/null; then
    print_success "✓ Valid $file_type"
    return 0
  else
    print_error "✗ Invalid $file_type"
    echo "Running detailed validation:"
    check-jsonschema --schemafile "$schema" "$file"
    return 1
  fi
}

# Validate all submissions in a directory
validate_submission_dir() {
  local dir="$1"
  local has_errors=0

  print_info "Validating submission directory: $dir"

  # Find and validate metrics.json files
  if find "$dir" -name "metrics.json" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      if ! validate_file "$file" "$METRICS_SCHEMA" "metrics file"; then
        has_errors=1
      fi
    done < <(find "$dir" -name "metrics.json" -type f -print0)
  else
    print_warning "No metrics.json files found in $dir"
  fi

  # Find and validate metadata.json files
  if find "$dir" -name "metadata.json" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      if ! validate_file "$file" "$METADATA_SCHEMA" "metadata file"; then
        has_errors=1
      fi
    done < <(find "$dir" -name "metadata.json" -type f -print0)
  else
    print_warning "No metadata.json files found in $dir"
  fi

  return $has_errors
}

# Show usage information
show_usage() {
  cat << EOF
CAPE Submission Validation

USAGE:
    cape submission validate [OPTIONS] [PATH]

DESCRIPTION:
    Validates submission files (metrics.json and metadata.json) against
    their respective JSON schemas to ensure proper format and completeness.

OPTIONS:
    -h, --help       Show this help message
    -a, --all        Validate all submissions in the submissions directory
    -s, --single     Validate a specific file (requires file path)

ARGUMENTS:
    PATH            Path to submission directory or specific file to validate
                    If not provided with --all, defaults to current directory

EXAMPLES:
    # Validate all submissions
    cape submission validate --all

    # Validate a specific submission directory
    cape submission validate submissions/fibonacci

    # Validate a specific file
    cape submission validate --single path/to/metrics.json

    # Validate current directory
    cape submission validate

SCHEMAS:
    - Metrics schema: submissions/TEMPLATE/metrics.schema.json
    - Metadata schema: submissions/TEMPLATE/metadata.schema.json

EOF
}

# Parse command line arguments
validate_all=false
validate_single=false
target_path=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -h | --help)
      show_usage
      exit 0
      ;;
    -a | --all)
      validate_all=true
      shift
      ;;
    -s | --single)
      validate_single=true
      shift
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

# Main validation logic
main() {
  check_jsonschema

  local exit_code=0

  if [[ "$validate_all" == true ]]; then
    print_info "Validating all submissions in $PROJECT_ROOT/submissions/"

    # Find all submission directories (excluding TEMPLATE)
    local submission_dirs=()
    while IFS= read -r -d '' dir; do
      local basename=$(basename "$dir")
      if [[ "$basename" != "TEMPLATE" ]]; then
        submission_dirs+=("$dir")
      fi
    done < <(find "$PROJECT_ROOT/submissions" -mindepth 1 -maxdepth 1 -type d -print0 2> /dev/null || true)

    if [[ ${#submission_dirs[@]} -eq 0 ]]; then
      print_warning "No submission directories found"
      exit 0
    fi

    for dir in "${submission_dirs[@]}"; do
      if ! validate_submission_dir "$dir"; then
        exit_code=1
      fi
      echo
    done

  elif [[ "$validate_single" == true ]]; then
    if [[ -z "$target_path" ]]; then
      print_error "File path required when using --single option"
      show_usage
      exit 1
    fi

    if [[ ! -f "$target_path" ]]; then
      print_error "File not found: $target_path"
      exit 1
    fi

    # Determine file type and appropriate schema
    local filename=$(basename "$target_path")
    case "$filename" in
      metrics.json)
        validate_file "$target_path" "$METRICS_SCHEMA" "metrics file"
        exit_code=$?
        ;;
      metadata.json)
        validate_file "$target_path" "$METADATA_SCHEMA" "metadata file"
        exit_code=$?
        ;;
      *)
        print_error "Unknown file type: $filename"
        print_info "Supported files: metrics.json, metadata.json"
        exit 1
        ;;
    esac

  else
    # Validate specific directory or current directory
    local dir_to_validate="${target_path:-$PWD}"

    if [[ ! -d "$dir_to_validate" ]]; then
      print_error "Directory not found: $dir_to_validate"
      exit 1
    fi

    validate_submission_dir "$dir_to_validate"
    exit_code=$?
  fi

  if [[ $exit_code -eq 0 ]]; then
    print_success "All validations passed!"
  else
    print_error "Some validations failed. Please check the output above."
  fi

  exit $exit_code
}

main "$@"
