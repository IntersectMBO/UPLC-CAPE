#!/usr/bin/env bash
set -euo pipefail

# List all submission folders for a given scenario
# Usage: cape submission list [scenario]

## Help
if [ $# -eq 1 ] && [ "$1" = "--help" ]; then
  echo "Usage: cape submission list [scenario]"
  echo "Example: cape submission list fibonacci"
  echo ""
  echo "Arguments:"
  echo "  scenario - (Optional) Show submissions for specific benchmark"
  echo ""
  echo "Without arguments, shows all submissions for every benchmark"
  exit 0
fi

if [ $# -gt 1 ]; then
  echo "Usage: cape submission list [scenario]"
  echo "Example: cape submission list fibonacci"
  exit 1
fi

if [ $# -eq 1 ]; then
  SCENARIO="$1"
else
  SCENARIO="" # show all
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
