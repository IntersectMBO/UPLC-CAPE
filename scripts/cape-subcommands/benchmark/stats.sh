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

# Cape Benchmark Stats - Generates JSON statistics for all benchmarks and submissions
# Usage: cape benchmark stats
#
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

# Function to format numbers for display
format_number() {
  local num="$1"
  if [ -z "$num" ] || [ "$num" = "null" ] || [ "$num" = "" ]; then
    echo "0"
    return
  fi

  # Use awk for number formatting
  echo "$num" | awk '{
    if ($1 >= 1000000000) {
      printf "%.2fB", $1/1000000000
    } else if ($1 >= 1000000) {
      printf "%.2fM", $1/1000000
    } else if ($1 >= 1000) {
      printf "%.2fK", $1/1000
    } else {
      print $1
    }
  }'
}

# Generate JSON data structure
temp_json="/tmp/cape_stats_$$_$(date +%s).json"

# Start JSON generation
cat > "$temp_json" << 'EOF'
{
  "generated_at": "",
  "benchmarks": []
}
EOF

# Update generated_at timestamp
jq --arg timestamp "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" '.generated_at = $timestamp' "$temp_json" > "${temp_json}.tmp" && mv "${temp_json}.tmp" "$temp_json"

# Get list of all benchmarks (as directories)
benchmarks=$(find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" -exec basename {} \; 2> /dev/null | sort)

for benchmark in $benchmarks; do
  # Skip TEMPLATE
  if [ "$benchmark" = "TEMPLATE" ]; then
    continue
  fi

  # Count submissions for this benchmark
  submission_count=0
  if [ -d "$PROJECT_ROOT/submissions/$benchmark" ]; then
    submission_count=$(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d ! -name "TEMPLATE" 2> /dev/null | wc -l)
  fi

  # Create benchmark object
  benchmark_json=$(jq -n \
    --arg name "$benchmark" \
    --argjson count "$submission_count" \
    '{
      name: $name,
      submission_count: $count,
      submissions: []
    }')

  # Process submissions for this benchmark
  if [ -d "$PROJECT_ROOT/submissions/$benchmark" ]; then
    while IFS= read -r -d '' submission_dir; do
      # Skip TEMPLATE
      if [[ "$(basename "$submission_dir")" == "TEMPLATE" ]]; then
        continue
      fi

      metadata_file="$submission_dir/metadata.json"
      metrics_file="$submission_dir/metrics.json"

      # Skip if files don't exist
      if [ ! -f "$metadata_file" ] || [ ! -f "$metrics_file" ]; then
        continue
      fi

      # Extract data from metadata.json using jq
      compiler_name=$(jq -r '.compiler.name // ""' "$metadata_file")
      compiler_version=$(jq -r '.compiler.version // ""' "$metadata_file")
      contributor_name=$(jq -r '.contributor.name // ""' "$metadata_file")

      # Extract data from metrics.json using jq
      timestamp=$(jq -r '.timestamp // ""' "$metrics_file")
      cpu_units=$(jq -r '.measurements.cpu_units // 0' "$metrics_file")
      memory_units=$(jq -r '.measurements.memory_units // 0' "$metrics_file")
      script_size_bytes=$(jq -r '.measurements.script_size_bytes // 0' "$metrics_file")
      term_size=$(jq -r '.measurements.term_size // 0' "$metrics_file")

      # Extract date from timestamp (YYYY-MM-DD)
      date_only=""
      if [ -n "$timestamp" ] && [ "$timestamp" != "null" ]; then
        date_only=$(echo "$timestamp" | cut -d'T' -f1)
      fi

      # Create submission object with nested metric structure
      submission_json=$(jq -n \
        --arg compiler "$compiler_name" \
        --arg version "$compiler_version" \
        --arg author "$contributor_name" \
        --arg date "$date_only" \
        --arg timestamp "$timestamp" \
        --argjson cpu_units "$cpu_units" \
        --arg cpu_formatted "$(format_number $cpu_units)" \
        --argjson memory_units "$memory_units" \
        --arg memory_formatted "$(format_number $memory_units)" \
        --argjson script_size "$script_size_bytes" \
        --argjson term_size "$term_size" \
        --arg submission_dir "$(basename "$submission_dir")" \
        '{
          compiler: $compiler,
          version: $version,
          author: $author,
          date: $date,
          timestamp: $timestamp,
          cpu_units: {
            value: $cpu_units,
            formatted: $cpu_formatted,
            is_best: false
          },
          memory_units: {
            value: $memory_units,
            formatted: $memory_formatted,
            is_best: false
          },
          script_size: {
            value: $script_size,
            formatted: ($script_size | tostring),
            is_best: false
          },
          term_size: {
            value: $term_size,
            formatted: ($term_size | tostring),
            is_best: false
          },
          submission_dir: $submission_dir
        }')

      # Add submission to benchmark
      benchmark_json=$(echo "$benchmark_json" | jq --argjson submission "$submission_json" '.submissions += [$submission]')

    done < <(find "$PROJECT_ROOT/submissions/$benchmark" -mindepth 1 -maxdepth 1 -type d -print0 2> /dev/null | sort -z)
  fi

  # Calculate best values for this benchmark if there are submissions
  if [ "$submission_count" -gt 0 ]; then
    # Extract all metric values from submissions using the nested structure
    cpu_values=$(echo "$benchmark_json" | jq -r '.submissions[].cpu_units.value')
    memory_values=$(echo "$benchmark_json" | jq -r '.submissions[].memory_units.value')
    script_values=$(echo "$benchmark_json" | jq -r '.submissions[].script_size.value')
    term_values=$(echo "$benchmark_json" | jq -r '.submissions[].term_size.value')

    # Find minimum values (best performance)
    best_cpu=$(echo "$cpu_values" | sort -n | head -1)
    best_memory=$(echo "$memory_values" | sort -n | head -1)
    best_script=$(echo "$script_values" | sort -n | head -1)
    best_term=$(echo "$term_values" | sort -n | head -1)

    # Update best values indicators in the nested structure
    benchmark_json=$(echo "$benchmark_json" | jq \
      --argjson best_cpu "$best_cpu" \
      --argjson best_memory "$best_memory" \
      --argjson best_script "$best_script" \
      --argjson best_term "$best_term" \
      '.submissions = (.submissions | map(
        .cpu_units.is_best = (.cpu_units.value == $best_cpu) |
        .memory_units.is_best = (.memory_units.value == $best_memory) |
        .script_size.is_best = (.script_size.value == $best_script) |
        .term_size.is_best = (.term_size.value == $best_term)
      ))')
  fi

  # Add benchmark to main JSON
  jq --argjson benchmark "$benchmark_json" '.benchmarks += [$benchmark]' "$temp_json" > "${temp_json}.tmp" && mv "${temp_json}.tmp" "$temp_json"
done

# Output pretty-printed JSON with proper color handling
if [ -t 1 ]; then
  # Output to terminal - use colors
  jq . "$temp_json"
else
  # Output to pipe/file - no colors
  jq --monochrome-output . "$temp_json"
fi

# Clean up temp file
rm -f "$temp_json"
