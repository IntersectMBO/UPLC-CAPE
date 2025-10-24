#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Resolve project root relative to this script (CWD-independent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Source shared helpers
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"

# Only detect root if PROJECT_ROOT is not already set (e.g., by test environment)
if [[ -z "${PROJECT_ROOT:-}" ]]; then
  cape_detect_root "$SCRIPT_DIR"
fi

# Cape Submission Aggregate - Generates CSV report of all benchmark submissions
# Usage: cape submission aggregate
#
# Output:
#   CSV format: benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size,submission_dir

# Early help
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h | --help | help)
      cape_render_help "${BASH_SOURCE[0]}"
      exit 0
      ;;
    *)
      echo "Error: Unknown argument '$1'" >&2
      echo "Usage: cape submission aggregate" >&2
      exit 1
      ;;
  esac
done

# Verify repo structure
if [ ! -d "$PROJECT_ROOT/submissions" ]; then
  echo "Error: Must be run within the project (submissions directory not found)" >&2
  echo "Detected PROJECT_ROOT=$PROJECT_ROOT" >&2
  exit 1
fi

# Output CSV header
echo "benchmark,timestamp,language,version,user,variant,cpu_units,memory_units,script_size_bytes,term_size,execution_fee_lovelace,reference_script_fee_lovelace,total_fee_lovelace,tx_memory_budget_pct,tx_cpu_budget_pct,block_memory_budget_pct,block_cpu_budget_pct,scripts_per_tx,scripts_per_block,submission_dir"

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

  # Extract data from metadata.json using basic JSON parsing (be tolerant to missing fields)
  compiler_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d '"' -f4 || true)
  compiler_version=$(grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d '"' -f4 || true)
  contributor_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | tail -1 | cut -d '"' -f4 || true)

  # If contributor name is the same as compiler name, it means contributor.name wasn't found
  if [ "${contributor_name:-}" = "${compiler_name:-}" ]; then
    contributor_name=""
  fi

  # Extract data from metrics.json using jq (new aggregated format)
  # Use sum strategy for aggregated measurements, fallback to direct values for backwards compatibility
  timestamp=$(jq -r '.timestamp // ""' "$metrics_file" 2> /dev/null || true)
  cpu_units=$(jq -r '.measurements.cpu_units.sum // .measurements.cpu_units // 0' "$metrics_file" 2> /dev/null || true)
  memory_units=$(jq -r '.measurements.memory_units.sum // .measurements.memory_units // 0' "$metrics_file" 2> /dev/null || true)
  script_size_bytes=$(jq -r '.measurements.script_size_bytes // 0' "$metrics_file" 2> /dev/null || true)
  term_size=$(jq -r '.measurements.term_size // 0' "$metrics_file" 2> /dev/null || true)

  # Extract derived metrics
  execution_fee_lovelace=$(jq -r '.measurements.execution_fee_lovelace // 0' "$metrics_file" 2> /dev/null || true)
  reference_script_fee_lovelace=$(jq -r '.measurements.reference_script_fee_lovelace // 0' "$metrics_file" 2> /dev/null || true)
  total_fee_lovelace=$(jq -r '.measurements.total_fee_lovelace // 0' "$metrics_file" 2> /dev/null || true)
  tx_memory_budget_pct=$(jq -r '.measurements.tx_memory_budget_pct // 0' "$metrics_file" 2> /dev/null || true)
  tx_cpu_budget_pct=$(jq -r '.measurements.tx_cpu_budget_pct // 0' "$metrics_file" 2> /dev/null || true)
  block_memory_budget_pct=$(jq -r '.measurements.block_memory_budget_pct // 0' "$metrics_file" 2> /dev/null || true)
  block_cpu_budget_pct=$(jq -r '.measurements.block_cpu_budget_pct // 0' "$metrics_file" 2> /dev/null || true)
  scripts_per_tx=$(jq -r '.measurements.scripts_per_tx // 0' "$metrics_file" 2> /dev/null || true)
  scripts_per_block=$(jq -r '.measurements.scripts_per_block // 0' "$metrics_file" 2> /dev/null || true)

  # Extract variant from submission directory name (format: Compiler_Version_Author[_Variant])
  # Split by underscore and check if 4th element exists
  variant=$(echo "$actual_submission_dir" | awk -F_ '{if (NF >= 4) print $4; else print "default"}')

  # Escape any commas in string fields and output CSV row
  benchmark=$(echo "$benchmark" | sed 's/,/\\,/g')
  timestamp=$(echo "$timestamp" | sed 's/,/\\,/g')
  compiler_name=$(echo "$compiler_name" | sed 's/,/\\,/g')
  compiler_version=$(echo "$compiler_version" | sed 's/,/\\,/g')
  contributor_name=$(echo "$contributor_name" | sed 's/,/\\,/g')
  variant=$(echo "$variant" | sed 's/,/\\,/g')
  actual_submission_dir=$(echo "$actual_submission_dir" | sed 's/,/\\,/g')

  echo "$benchmark,$timestamp,$compiler_name,$compiler_version,$contributor_name,$variant,$cpu_units,$memory_units,$script_size_bytes,$term_size,$execution_fee_lovelace,$reference_script_fee_lovelace,$total_fee_lovelace,$tx_memory_budget_pct,$tx_cpu_budget_pct,$block_memory_budget_pct,$block_cpu_budget_pct,$scripts_per_tx,$scripts_per_block,$actual_submission_dir"
done < <(find "$PROJECT_ROOT/submissions" -name "metadata.json" -path "*/[!T]*/*" -print0 | sort -z)
