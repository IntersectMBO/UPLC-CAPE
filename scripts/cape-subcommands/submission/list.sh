#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Resolve project root relative to this script (CWD-independent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"

# List all submission folders for a given scenario
# Usage: cape submission list [scenario]

# Check for --help first (before getopts processes it)
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Parse options
OPTIND=1
MODE=""
POSITIONAL_ARGS=()
while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help)
      cape_render_help "${BASH_SOURCE[0]}"
      exit 0
      ;;
    --mode)
      MODE="$2"
      shift 2
      ;;
    -*)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1")
      shift
      ;;
  esac
done

# Restore positional arguments
set -- "${POSITIONAL_ARGS[@]}"

if [ $# -gt 1 ]; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 1
fi

# Validate mode if provided
if [ -n "$MODE" ] && [ "$MODE" != "base" ] && [ "$MODE" != "open" ]; then
  echo "Error: Invalid mode '$MODE'. Must be 'base' or 'open'." >&2
  exit 1
fi

if [ $# -eq 1 ]; then
  SCENARIO="$1"
else
  SCENARIO="" # show all
fi

# Helper function to filter submissions by mode
filter_submissions() {
  local mode_filter="$1"
  if [ -z "$mode_filter" ]; then
    cat # no filter, pass through
  elif [ "$mode_filter" = "base" ]; then
    grep '_base$' || true
  elif [ "$mode_filter" = "open" ]; then
    # Match: _open$ (no slug), _open_slug, or legacy format (no suffix)
    grep -E '(_open$|_open_|^[^_]+_[^_]+_[^_]+$)' || true
  fi
}

# Helper function to detect submission mode from directory name
get_submission_mode() {
  local dirname="$1"
  if [[ "$dirname" =~ _base$ ]]; then
    echo "base"
  elif [[ "$dirname" =~ _open($|_) ]]; then
    echo "open"
  else
    # Legacy format: treat as open mode
    echo "legacy-open"
  fi
}

# Helper function to group and display submissions by mode
display_grouped_by_mode() {
  local submissions="$1"
  local indent="$2"

  if [ -z "$submissions" ]; then
    echo "${indent}(none)"
    return
  fi

  # Separate base, open, and legacy submissions
  local base_subs
  local open_subs
  local legacy_subs
  base_subs=$(echo "$submissions" | grep '_base$' || true)
  open_subs=$(echo "$submissions" | grep -E '_open($|_)' || true)
  legacy_subs=$(echo "$submissions" | grep -v '_base$' | grep -v -E '_open($|_)' || true)

  if [ -n "$base_subs" ]; then
    echo "${indent}Base mode:"
    echo "$base_subs" | sed "s/^/${indent}  /"
  fi

  if [ -n "$open_subs" ] || [ -n "$legacy_subs" ]; then
    echo "${indent}Open mode:"
    if [ -n "$open_subs" ]; then
      echo "$open_subs" | sed "s/^/${indent}  /"
    fi
    if [ -n "$legacy_subs" ]; then
      echo "$legacy_subs" | sed "s/^/${indent}  /" | sed "s/$/ (legacy)/"
    fi
  fi
}

if [ -n "$SCENARIO" ]; then
  # Show submissions for specific scenario
  SCENARIO_DIR="$PROJECT_ROOT/submissions/$SCENARIO"

  if [ -n "$MODE" ]; then
    echo "Submissions for benchmark '$SCENARIO' (mode: $MODE):"
  else
    echo "Submissions for benchmark '$SCENARIO' (all modes):"
  fi

  if [ -d "$SCENARIO_DIR" ] && find "$SCENARIO_DIR" -mindepth 1 -maxdepth 1 -type d | grep -q .; then
    submissions=$(find "$SCENARIO_DIR" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | filter_submissions "$MODE")

    if [ -z "$submissions" ]; then
      echo "  (none)"
    elif [ -z "$MODE" ]; then
      # Group by mode when no filter specified
      display_grouped_by_mode "$submissions" "  "
    else
      # Just list them when filtered
      echo "$submissions" | sed 's/^/  /'
    fi
  else
    echo "  (none)"
  fi
else
  # Show all submissions
  if [ -n "$MODE" ]; then
    echo "All submissions (mode: $MODE):"
  else
    echo "All submissions (all modes):"
  fi

  while IFS= read -r scenario_dir; do
    scenario="$(basename "$scenario_dir")"
    # Skip the TEMPLATE directory
    if [ "$scenario" = "TEMPLATE" ]; then
      continue
    fi
    echo "  $scenario:"

    if find "$scenario_dir" -mindepth 1 -maxdepth 1 -type d | grep -q .; then
      submissions=$(find "$scenario_dir" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | filter_submissions "$MODE")

      if [ -z "$submissions" ]; then
        echo "    (none)"
      elif [ -z "$MODE" ]; then
        # Group by mode when no filter specified
        display_grouped_by_mode "$submissions" "    "
      else
        # Just list them when filtered
        echo "$submissions" | sed 's/^/    /'
      fi
    else
      echo "    (none)"
    fi
  done < <(find "$PROJECT_ROOT/submissions" -mindepth 1 -maxdepth 1 -type d)
fi
