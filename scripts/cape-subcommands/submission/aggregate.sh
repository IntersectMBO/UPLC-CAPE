#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Resolve project root relative to this script (CWD-independent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Source shared helpers
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"

# Cape Submission Aggregate - Generates CSV report of all benchmark submissions
# Usage: cape submission aggregate

# Early help
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Verify repo structure
if [ ! -d "$PROJECT_ROOT/submissions" ]; then
  echo "Error: Must be run within the project (submissions directory not found)" >&2
  echo "Detected PROJECT_ROOT=$PROJECT_ROOT" >&2
  exit 1
fi

# Output CSV header
echo "benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir"

# Process all submissions (NUL-delimited for filename safety)
while IFS= read -r -d '' metadata_file; do
  # Extract paths
  submission_dir="$(dirname "$metadata_file")"
  metrics_file="$submission_dir/metrics.json"

  # Skip if metrics file doesn't exist
  if [ ! -f "$metrics_file" ]; then
    echo "Warning: No metrics.json found for $submission_dir" >&2
    continue
  fi

  # Extract benchmark name from path (submissions/benchmark/compiler_version_user/)
  benchmark="$(basename "$(dirname "$submission_dir")")"

  # Extract actual submission directory name
  actual_submission_dir="$(basename "$submission_dir")"

  # Extract data from metadata.json using basic JSON parsing
  compiler_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d '"' -f4)
  compiler_version=$(grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d '"' -f4)
  contributor_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | tail -1 | cut -d '"' -f4)

  # If contributor name is the same as compiler name, it means contributor.name wasn't found
  if [ "$contributor_name" = "$compiler_name" ]; then
    contributor_name=""
  fi

  # Extract data from metrics.json
  timestamp=$(grep -o '"timestamp"[[:space:]]*:[[:space:]]*"[^"]*"' "$metrics_file" | cut -d '"' -f4)
  cpu_units=$(grep -o '"cpu_units"[[:space:]]*:[[:space:]]*[0-9]*' "$metrics_file" | grep -o '[0-9]*$')
  memory_units=$(grep -o '"memory_units"[[:space:]]*:[[:space:]]*[0-9]*' "$metrics_file" | grep -o '[0-9]*$')
  script_size_bytes=$(grep -o '"script_size_bytes"[[:space:]]*:[[:space:]]*[0-9]*' "$metrics_file" | grep -o '[0-9]*$')
  term_size=$(grep -o '"term_size"[[:space:]]*:[[:space:]]*[0-9]*' "$metrics_file" | grep -o '[0-9]*$')

  # Escape any commas in string fields and output CSV row
  benchmark=$(echo "$benchmark" | sed 's/,/\\,/g')
  timestamp=$(echo "$timestamp" | sed 's/,/\\,/g')
  compiler_name=$(echo "$compiler_name" | sed 's/,/\\,/g')
  compiler_version=$(echo "$compiler_version" | sed 's/,/\\,/g')
  contributor_name=$(echo "$contributor_name" | sed 's/,/\\,/g')
  actual_submission_dir=$(echo "$actual_submission_dir" | sed 's/,/\\,/g')

  echo "$benchmark,$timestamp,$compiler_name,$compiler_version,$contributor_name,$cpu_units,$memory_units,$script_size_bytes,$term_size,$actual_submission_dir"
done < <(find "$PROJECT_ROOT/submissions" -name "metadata.json" -path "*/[!T]*/*" -print0 | sort -z)
