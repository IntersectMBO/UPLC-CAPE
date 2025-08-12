#!/usr/bin/env bash
set -euo pipefail

## Benchmark listing / details
# Usage:
#   cape benchmark list            # prints benchmark names only (grouped)
#   cape benchmark <name>          # shows detailed info (Overview + submissions)
#   cape benchmark list <name>     # same as above

## Help
if [ $# -eq 1 ] && [ "$1" = "--help" ]; then
  cat <<EOF
Usage:
  cape benchmark list [benchmark]
  cape benchmark [benchmark]

Description:
  'cape benchmark list' outputs only benchmark names (one per line).
  Benchmarks are defined by a primary markdown file 'scenarios/<name>.md'.
  Supplementary files named '<name>-*.md' are grouped under the same benchmark
  and not listed separately.

Details:
  'cape benchmark <name>' shows the full Overview section plus submission info.

Examples:
  cape benchmark list
  cape benchmark fibonacci
  cape benchmark list fibonacci
EOF
  exit 0
fi

if [ $# -gt 1 ]; then
  echo "Usage: cape benchmark list [benchmark]" >&2
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

# Build grouped benchmark set
discover_benchmarks() {
  declare -gA MAIN_FILES=()
  declare -gA HAS_SUPPLEMENTARY=()
  if [ -d "scenarios" ]; then
    for f in scenarios/*.md; do
      [ -f "$f" ] || continue
      local base
      base=$(basename "$f" .md)
      [ "$base" = "TEMPLATE" ] && continue
      if [[ "$base" == *-* ]]; then
        local root=${base%%-*}
        HAS_SUPPLEMENTARY[$root]=1
      else
        # First main wins; ignore duplicates but warn
        if [ -n "${MAIN_FILES[$base]:-}" ]; then
          echo "Warning: duplicate main benchmark file for '$base' -> $f (ignored)" >&2
        else
          MAIN_FILES[$base]="$f"
        fi
      fi
    done
  fi
}

# Return sorted unique benchmark names
sorted_benchmark_names() {
  {
    for k in "${!MAIN_FILES[@]}"; do echo "$k"; done
    for k in "${!HAS_SUPPLEMENTARY[@]}"; do echo "$k"; done
  } | sort -u
}

if [ $# -eq 1 ]; then BENCHMARK="$1"; else BENCHMARK=""; fi

discover_benchmarks

# Render markdown (use glow if available and not disabled)
render_markdown() {
  if [ -n "${CAPE_NO_GLOW:-}" ]; then
    cat
    return
  fi
  if command -v glow >/dev/null 2>&1; then
    # Use a safe theme; fallback to cat on error
    glow - 2>/dev/null || cat
  else
    cat
  fi
}

if [ -n "$BENCHMARK" ]; then
  # Detailed view
  local_file="scenarios/$BENCHMARK.md"
  if [ -f "$local_file" ]; then
    canonical="$local_file"
  else
    # fallback: first supplementary
    for f in scenarios/$BENCHMARK-*.md; do
      if [ -f "$f" ]; then canonical="$f"; break; fi
    done
  fi
  if [ -z "${canonical:-}" ]; then
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
  if [ -d "submissions/$BENCHMARK" ]; then
    submission_count=$(find "submissions/$BENCHMARK" -maxdepth 1 -type d ! -path "submissions/$BENCHMARK" | wc -l)
  else
    submission_count=0
  fi
  echo "Submissions ($submission_count):"
  if [ $submission_count -gt 0 ]; then
    find "submissions/$BENCHMARK" -maxdepth 1 -type d ! -path "submissions/$BENCHMARK" | head -5 | while read d; do
      echo "  - $(basename "$d")"
    done
    if [ $submission_count -gt 5 ]; then
      echo "  ... and $((submission_count - 5)) more"
    fi
  else
    echo "  (none)"
  fi
else
  # Plain list
  sorted_benchmark_names
fi
