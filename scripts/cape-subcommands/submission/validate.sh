#!/usr/bin/env bash

# CAPE Submission Validation Script
# Validates submission files against JSON schemas

set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"

# Schema file paths
METRICS_SCHEMA="$PROJECT_ROOT/submissions/TEMPLATE/metrics.schema.json"
METADATA_SCHEMA="$PROJECT_ROOT/submissions/TEMPLATE/metadata.schema.json"

# Early help handling via shared helper
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Check if check-jsonschema is available
check_jsonschema() {
  cape_require_cmd check-jsonschema "Run 'nix develop' to get check-jsonschema."
}

# Validate a single file against a schema
validate_file() {
  local file="$1"
  local schema="$2"
  local file_type="$3"
  local rel_file
  rel_file="$(cape_relpath "$file")"

  if [[ ! -f "$file" ]]; then
    cape_warn "File not found: $rel_file"
    return 1
  fi

  if [[ ! -f "$schema" ]]; then
    cape_error "Schema not found: $(cape_relpath "$schema")"
    return 1
  fi

  cape_info "Validating $file_type: $rel_file"

  # Suppress default 'ok -- validation done' output on success
  if check-jsonschema --schemafile "$schema" "$file" > /dev/null 2>&1; then
    # Capitalize first letter and change message
    local capitalized_type="$(echo "${file_type:0:1}" | tr '[:lower:]' '[:upper:]')${file_type:1}"
    cape_success "✓ $capitalized_type matches schema ($rel_file)"
    return 0
  else
    cape_error "✗ $file_type invalid ($rel_file)"
    echo "Running detailed validation:"
    check-jsonschema --schemafile "$schema" "$file"
    return 1
  fi
}

# Validate directory name format based on metadata.json contents
validate_directory_name() {
  local submission_dir="$1"
  local metadata_file="$2"
  local expected_name
  local actual_name

  actual_name="$(basename "$submission_dir")"

  # Extract values from metadata.json using basic JSON parsing
  local compiler_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d'"' -f4)
  local compiler_version=$(grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d'"' -f4)
  local contributor_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | tail -1 | cut -d'"' -f4)

  # If contributor name is the same as compiler name, it means contributor.name wasn't found
  if [ "$contributor_name" = "$compiler_name" ]; then
    contributor_name=""
  fi

  # Construct expected directory name
  expected_name="${compiler_name}_${compiler_version}_${contributor_name}"

  if [ "$actual_name" = "$expected_name" ]; then
    cape_success "✓ Directory name matches metadata ($actual_name)"
    return 0
  else
    cape_error "✗ Directory name mismatch"
    cape_error "  Expected: $expected_name"
    cape_error "  Actual:   $actual_name"
    return 1
  fi
}

# Validate all submissions in a directory
validate_submission_dir() {
  local dir="$1"
  local has_errors=0
  local rel_dir
  rel_dir="$(cape_relpath "$dir")"

  cape_info "Validating submission directory: $rel_dir"

  # Find and validate metrics.json files
  if find "$dir" -name "metrics.json" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      if ! validate_file "$file" "$METRICS_SCHEMA" "metrics file"; then
        has_errors=1
      fi
    done < <(find "$dir" -name "metrics.json" -type f -print0)
  else
    cape_warn "No metrics.json files found in $rel_dir"
  fi

  # Find and validate metadata.json files
  if find "$dir" -name "metadata.json" -type f | grep -q .; then
    while IFS= read -r -d '' file; do
      if ! validate_file "$file" "$METADATA_SCHEMA" "metadata file"; then
        has_errors=1
      else
        # If metadata validation passed, also check directory name format
        local submission_dir="$(dirname "$file")"
        # Check if this looks like an individual submission directory (contains metadata.json directly)
        if [[ -f "$submission_dir/metadata.json" && -f "$submission_dir/metrics.json" ]]; then
          if ! validate_directory_name "$submission_dir" "$file"; then
            has_errors=1
          fi
        fi
      fi
    done < <(find "$dir" -name "metadata.json" -type f -print0)
  else
    cape_warn "No metadata.json files found in $rel_dir"
  fi

  return $has_errors
}

# Parse command line arguments
validate_all=false
validate_single=false
target_path=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -a | --all)
      validate_all=true
      shift
      ;;
    -s | --single)
      validate_single=true
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

# Enforce explicit mode or path to avoid accidental no-op validations
if [[ "$validate_all" == false && "$validate_single" == false && -z "${target_path}" ]]; then
  cape_error "No mode or path specified."
  cape_render_help "${BASH_SOURCE[0]}"
  exit 1
fi

# Main validation logic
main() {
  check_jsonschema

  local exit_code=0

  if [[ "$validate_all" == true ]]; then
    cape_info "Validating all submissions in $(cape_relpath "$PROJECT_ROOT/submissions/")"

    # Find all submission directories (excluding TEMPLATE)
    local submission_dirs=()
    while IFS= read -r -d '' dir; do
      local basename=$(basename "$dir")
      if [[ "$basename" != "TEMPLATE" ]]; then
        submission_dirs+=("$dir")
      fi
    done < <(find "$PROJECT_ROOT/submissions" -mindepth 1 -maxdepth 1 -type d -print0 2> /dev/null || true)

    if [[ ${#submission_dirs[@]} -eq 0 ]]; then
      cape_warn "No submission directories found"
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
      cape_error "File path required when using --single option"
      cape_render_help "${BASH_SOURCE[0]}"
      exit 1
    fi

    if [[ ! -f "$target_path" ]]; then
      cape_error "File not found: $(cape_relpath "$target_path")"
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
        cape_error "Unknown file type: $filename"
        cape_info "Supported files: metrics.json, metadata.json"
        exit 1
        ;;
    esac

  else
    # Validate specific directory or current directory
    local dir_to_validate="${target_path:-$PWD}"

    if [[ ! -d "$dir_to_validate" ]]; then
      cape_error "Directory not found: $(cape_relpath "$dir_to_validate")"
      exit 1
    fi

    validate_submission_dir "$dir_to_validate"
    exit_code=$?
  fi

  if [[ $exit_code -eq 0 ]]; then
    cape_success "All validations passed!"
  else
    cape_error "Some validations failed. Please check the output above."
  fi

  exit $exit_code
}

main "$@"
