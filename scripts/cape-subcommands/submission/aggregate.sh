#!/usr/bin/env bash
set -euo pipefail

# Cape Submission Aggregate - Generates CSV report of all benchmark submissions
# Usage: cape submission aggregate

show_help() {
  cat << EOF
Usage: cape submission aggregate

Discovers all benchmark submissions and outputs CSV containing a row for each one:
- benchmark: The benchmark scenario name
- timestamp: ISO-8601 timestamp from metrics
- language: Compiler name (derived from metadata)
- version: Compiler version
- user: Contributor name
- cpu_units: CPU execution units consumed
- memory_units: Memory units consumed
- script_size_bytes: Size of serialized UPLC script in bytes
- term_size: Number of AST nodes in the UPLC term

Output is printed to stdout in CSV format with headers.

Examples:
  cape submission aggregate > results.csv
  cape submission aggregate | head -10
EOF
}

# Check for help flag
if [[ "${1:-}" =~ ^(-h|--help|help)$ ]]; then
  show_help
  exit 0
fi

# Find project root (should already be set by cape.sh)
if [ ! -d "submissions" ]; then
  echo "Error: Must be run from project root (submissions directory not found)" >&2
  exit 1
fi

# Output CSV header
echo "benchmark,timestamp,language,version,user,cpu_units,memory_units,script_size_bytes,term_size"

# Process all submissions
find submissions -name "metadata.json" -path "*/[!T]*/*" | sort | while read -r metadata_file; do
  # Extract paths
  submission_dir=$(dirname "$metadata_file")
  metrics_file="$submission_dir/metrics.json"

  # Skip if metrics file doesn't exist
  if [ ! -f "$metrics_file" ]; then
    echo "Warning: No metrics.json found for $submission_dir" >&2
    continue
  fi

  # Extract benchmark name from path (submissions/benchmark/compiler_version_user/)
  benchmark=$(basename "$(dirname "$submission_dir")")

  # Extract data from metadata.json using basic JSON parsing
  compiler_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d'"' -f4)
  compiler_version=$(grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | head -1 | cut -d'"' -f4)
  contributor_name=$(grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$metadata_file" | tail -1 | cut -d'"' -f4)

  # If contributor name is the same as compiler name, it means contributor.name wasn't found
  if [ "$contributor_name" = "$compiler_name" ]; then
    contributor_name=""
  fi

  # Extract data from metrics.json
  timestamp=$(grep -o '"timestamp"[[:space:]]*:[[:space:]]*"[^"]*"' "$metrics_file" | cut -d'"' -f4)
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

  echo "$benchmark,$timestamp,$compiler_name,$compiler_version,$contributor_name,$cpu_units,$memory_units,$script_size_bytes,$term_size"
done
