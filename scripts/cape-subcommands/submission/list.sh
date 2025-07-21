#!/usr/bin/env bash
set -euo pipefail

# List all submission folders for a given scenario
# Usage: cape submission list [scenario]

# Check for help flag
if [ $# -eq 1 ] && [ "$1" = "--help" ]; then
  echo "Usage: cape submission list [scenario]"
  echo "Example: cape submission list fibonacci"
  echo "Note: If scenario is not provided, you can choose interactively"
  echo ""
  echo "Arguments:"
  echo "  scenario - (Optional) Show submissions for specific benchmark"
  echo ""
  echo "Without arguments, shows all submissions or prompts for selection"
  exit 0
fi

# Function to prompt for scenario if not provided
prompt_for_scenario() {
  echo "Available benchmarks with submissions:" >&2
  local has_submissions=false

  for scenario_dir in submissions/*/; do
    if [ -d "$scenario_dir" ]; then
      scenario=$(basename "$scenario_dir")
      # Skip the TEMPLATE directory
      if [ "$scenario" = "TEMPLATE" ]; then
        continue
      fi
      if [ "$(ls -A "$scenario_dir" 2> /dev/null)" ]; then
        echo "  $scenario" >&2
        has_submissions=true
      fi
    fi
  done

  if [ "$has_submissions" = false ]; then
    echo "  (none)" >&2
    return 1 # Signal no submissions found
  fi

  echo "" >&2
  echo -n "Enter benchmark name to view its submissions (or press Enter to see all): " >&2
  echo -n "> " >&2
  read -r scenario_name

  echo "$scenario_name"
}

if [ $# -gt 1 ]; then
  echo "Usage: cape submission list [scenario]"
  echo "Example: cape submission list fibonacci"
  echo "Note: If scenario is not provided, you can choose interactively"
  exit 1
fi

if [ $# -eq 1 ]; then
  SCENARIO="$1"
elif [ $# -eq 0 ]; then
  # No scenario provided - prompt user or show all
  SCENARIO=$(prompt_for_scenario) || {
    # No submissions found, exit gracefully
    exit 0
  }

  # If empty input, show all submissions
  if [ -z "$SCENARIO" ]; then
    SCENARIO=""
  fi
fi

if [ -n "$SCENARIO" ]; then
  # Show submissions for specific scenario
  SCENARIO_DIR="submissions/$SCENARIO"

  if [ ! -d "$SCENARIO_DIR" ]; then
    echo "Error: Benchmark '${SCENARIO}' not found in submissions/"
    echo "Available benchmarks:"
    for scenario_dir in submissions/*/; do
      if [ -d "$scenario_dir" ]; then
        scenario=$(basename "$scenario_dir")
        if [ "$scenario" != "TEMPLATE" ]; then
          echo "  $scenario"
        fi
      fi
    done
    exit 1
  fi

  echo "Submissions for benchmark '$SCENARIO':"
  if [ -d "$SCENARIO_DIR" ] && [ "$(ls -A "$SCENARIO_DIR" 2> /dev/null)" ]; then
    ls -1 "$SCENARIO_DIR" | sed 's/^/  /'
  else
    echo "  (none)"
  fi
else
  # Show all submissions
  echo "All submissions:"
  for scenario_dir in submissions/*/; do
    if [ -d "$scenario_dir" ]; then
      scenario=$(basename "$scenario_dir")
      # Skip the TEMPLATE directory
      if [ "$scenario" = "TEMPLATE" ]; then
        continue
      fi
      echo "  $scenario:"
      if [ "$(ls -A "$scenario_dir" 2> /dev/null)" ]; then
        ls -1 "$scenario_dir" | sed 's/^/    /'
      else
        echo "    (none)"
      fi
    fi
  done
fi
