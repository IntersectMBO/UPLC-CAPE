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
POSITIONAL_ARGS=()
while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help)
      cape_render_help "${BASH_SOURCE[0]}"
      exit 0
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

if [ $# -eq 1 ]; then
  SCENARIO="$1"
else
  SCENARIO="" # show all
fi

if [ -n "$SCENARIO" ]; then
  # Show submissions for specific scenario
  SCENARIO_DIR="$PROJECT_ROOT/submissions/$SCENARIO"

  echo "Submissions for benchmark '$SCENARIO':"

  if [ -d "$SCENARIO_DIR" ] && find "$SCENARIO_DIR" -mindepth 1 -maxdepth 1 -type d | grep -q .; then
    find "$SCENARIO_DIR" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort | sed 's/^/  /'
  else
    echo "  (none)"
  fi
else
  # Show all submissions
  echo "All submissions:"

  while IFS= read -r scenario_dir; do
    scenario="$(basename "$scenario_dir")"
    # Skip the TEMPLATE directory
    if [ "$scenario" = "TEMPLATE" ]; then
      continue
    fi
    echo "  $scenario:"

    if find "$scenario_dir" -mindepth 1 -maxdepth 1 -type d | grep -q .; then
      find "$scenario_dir" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort | sed 's/^/    /'
    else
      echo "    (none)"
    fi
  done < <(find "$PROJECT_ROOT/submissions" -mindepth 1 -maxdepth 1 -type d | sort)
fi
