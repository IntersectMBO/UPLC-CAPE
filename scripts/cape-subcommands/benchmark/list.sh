#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

## Benchmark listing / details
# Usage:
#   cape benchmark list            # prints benchmark names only (grouped)
#   cape benchmark <name>          # shows detailed info (Overview + submissions)
#   cape benchmark list <name>     # same as above

# Optional command check (non-fatal)
has_cmd() { command -v "$1" > /dev/null 2>&1; }

# Resolve project root relative to this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Source shared helpers and detect root
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

# Show help text via gomplate
render_help() {
  cape_render_help "${BASH_SOURCE[0]}"
}

# Show help text
show_help() {
  render_help
}

# Check for help first
if cape_help_requested "$@"; then
  show_help
  exit 0
fi

# Parse options (support -h)
OPTIND=1
while getopts ":h" opt; do
  case "$opt" in
    h)
      show_help
      exit 0
      ;;
    \?)
      echo "Unknown option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND - 1))

if [ $# -gt 1 ]; then
  show_help
  exit 1
fi

########################################
# Helpers
########################################

# Extract full Overview section (without the header line)
extract_overview() {
  local file="$1"
  [ -f "$file" ] || return 0
  # sed script: find line starting with '## Overview', then print until next '## ' header or EOF
  # Remove the first line (the header itself)
  sed -n '/^##[ ][ ]*Overview[ ]*$/,/^## /p' "$file" \
    | sed '1d' \
    | sed '/^## /d' \
    | sed '/./,$!d'
}

# Build benchmark list - look for directories with scenario files
discover_benchmarks() {
  declare -gA BENCHMARK_FILES=()
  if [ -d "$PROJECT_ROOT/scenarios" ]; then
    for d in "$PROJECT_ROOT"/scenarios/*/; do
      [ -d "$d" ] || continue
      local base
      base=$(basename "$d")
      [ "$base" = "TEMPLATE" ] && continue

      # Look for scenario file: prefer scenario_name.md, fallback to README.md
      local scenario_file=""
      if [ -f "$d/$base.md" ]; then
        scenario_file="$d/$base.md"
      elif [ -f "$d/README.md" ]; then
        scenario_file="$d/README.md"
      else
        continue # Skip directories without scenario files
      fi

      BENCHMARK_FILES[$base]="$scenario_file"
    done
  fi
}

# Return sorted benchmark names
sorted_benchmark_names() {
  for k in "${!BENCHMARK_FILES[@]}"; do echo "$k"; done | sort
}

# Determine benchmark positional (if any)
if [ $# -eq 1 ]; then BENCHMARK="$1"; else BENCHMARK=""; fi

discover_benchmarks

# Render markdown (use glow if available and not disabled)
render_markdown() {
  if [ -n "${CAPE_NO_GLOW:-}" ]; then
    cat
    return
  fi
  if command -v glow > /dev/null 2>&1; then
    # Use a safe theme; fallback to cat on error
    glow - 2> /dev/null || cat
  else
    cat
  fi
}

if [ -n "$BENCHMARK" ]; then
  # Detailed view
  canonical="${BENCHMARK_FILES[$BENCHMARK]:-}"
  if [ -z "$canonical" ]; then
    echo "Error: Benchmark '$BENCHMARK' not found" >&2
    sorted_benchmark_names | sed 's/^/  /' >&2
    exit 1
  fi
  overview=$(extract_overview "$canonical") || true
  if [ -n "$overview" ]; then
    # Strip horizontal rule lines (---) before rendering
    cleaned_overview=$(printf '%s\n' "$overview" | sed '/^[[:space:]]*---[[:space:]]*$/d')
    printf '%s\n' "$cleaned_overview" | render_markdown
    echo
  else
    echo "(no Overview section found)"
    echo
  fi
  # submissions summary
  if [ -d "$PROJECT_ROOT/submissions/$BENCHMARK" ]; then
    submission_count=$(find "$PROJECT_ROOT/submissions/$BENCHMARK" -maxdepth 1 -type d ! -path "$PROJECT_ROOT/submissions/$BENCHMARK" | wc -l)
  else
    submission_count=0
  fi
  echo "Submissions ($submission_count):"
  if [ $submission_count -gt 0 ]; then
    # List all submissions safely
    while IFS= read -r -d '' d; do
      echo "  - $(basename "$d")"
    done < <(find "$PROJECT_ROOT/submissions/$BENCHMARK" -maxdepth 1 -type d ! -path "$PROJECT_ROOT/submissions/$BENCHMARK" -print0)
  else
    echo "  (none)"
  fi
else
  # Plain list
  sorted_benchmark_names
fi
