#!/usr/bin/env bash
set -euo pipefail

# List all available benchmarks with their descriptions
# Usage: cape benchmark list [benchmark-name]

# Check for help flag
if [ $# -eq 1 ] && [ "$1" = "--help" ]; then
  echo "Usage: cape benchmark list [benchmark-name]"
  echo "       cape benchmark [benchmark-name]"
  echo "Example: cape benchmark list fibonacci"
  echo "Note: If benchmark-name is not provided, you can choose interactively"
  echo ""
  echo "Arguments:"
  echo "  benchmark-name - (Optional) Show detailed info for specific benchmark"
  echo ""
  echo "Without arguments, shows all available benchmarks or prompts for selection"
  exit 0
fi

# Function to prompt for benchmark if not provided
prompt_for_benchmark() {
  echo "Available benchmarks:" >&2
  local has_benchmarks=false

  if [ -d "scenarios" ]; then
    for benchmark_file in scenarios/*.md; do
      if [ -f "$benchmark_file" ]; then
        benchmark_name=$(basename "$benchmark_file" .md)
        if [ "$benchmark_name" != "TEMPLATE" ]; then
          echo "  $benchmark_name" >&2
          has_benchmarks=true
        fi
      fi
    done
  fi

  if [ "$has_benchmarks" = false ]; then
    echo "  (none)" >&2
    return 1 # Signal to exit
  fi

  echo "" >&2
  echo -n "Enter benchmark name for detailed information (or press Enter to see all): " >&2
  echo -n "> " >&2
  read -r benchmark_name

  echo "$benchmark_name"
}

if [ $# -gt 1 ]; then
  echo "Usage: cape benchmark list [benchmark-name]"
  echo "       cape benchmark [benchmark-name]"
  echo "Example: cape benchmark list fibonacci"
  echo "Note: If benchmark-name is not provided, you can choose interactively"
  exit 1
fi

# Function to extract description from benchmark markdown file
extract_description() {
  local benchmark_file="$1"
  if [ -f "$benchmark_file" ]; then
    sed -n '/^## Overview/,/^##/{/^## Overview/d; /^##/d; /^$/d; p; }' "$benchmark_file" | head -1
  fi
}

if [ $# -eq 1 ]; then
  # Show detailed information about a specific benchmark
  BENCHMARK="$1"
elif [ $# -eq 0 ]; then
  # No benchmark provided - prompt user or show all
  BENCHMARK=$(prompt_for_benchmark) || {
    # If prompt returns error (no benchmarks), exit
    exit 0
  }

  # If empty input, show all benchmarks
  if [ -z "$BENCHMARK" ]; then
    BENCHMARK=""
  fi
fi

if [ -n "$BENCHMARK" ]; then
  # Show detailed information about the specific benchmark
  BENCHMARK_FILE="scenarios/$BENCHMARK.md"

  if [ ! -f "$BENCHMARK_FILE" ]; then
    echo "Error: Benchmark '$BENCHMARK' not found"
    echo ""
    echo "Available benchmarks:"
    if [ -d "scenarios" ]; then
      for benchmark_file in scenarios/*.md; do
        if [ -f "$benchmark_file" ]; then
          benchmark_name=$(basename "$benchmark_file" .md)
          if [ "$benchmark_name" != "TEMPLATE" ]; then
            echo "  $benchmark_name"
          fi
        fi
      done
    fi
    exit 1
  fi

  # Show detailed information about the specific benchmark
  benchmark_name=$(basename "$BENCHMARK_FILE" .md)
  echo "🎯 Benchmark: $benchmark_name"
  echo ""

  # Extract and show description
  description=$(extract_description "$BENCHMARK_FILE")
  if [ -n "$description" ]; then
    echo "Description:"
    echo "$description" | fold -s -w 70 | sed 's/^/  /'
    echo ""
  fi

  # Show submission count
  if [ -d "submissions/$benchmark_name" ]; then
    submission_count=$(find "submissions/$benchmark_name" -maxdepth 1 -type d ! -path "submissions/$benchmark_name" | wc -l)
    echo "📊 Submissions: $submission_count"

    if [ $submission_count -gt 0 ]; then
      echo ""
      echo "Recent submissions:"
      find "submissions/$benchmark_name" -maxdepth 1 -type d ! -path "submissions/$benchmark_name" \
        | head -5 | while read submission_dir; do
        submission_name=$(basename "$submission_dir")
        echo "  • $submission_name"
      done

      if [ $submission_count -gt 5 ]; then
        echo "  ... and $((submission_count - 5)) more"
      fi
    fi
  else
    echo "📊 Submissions: 0"
    echo ""
    echo "💡 To create a submission: cape submission new $benchmark_name <compiler> <version> <contributor>"
  fi

else
  # Show all available benchmarks
  echo "📋 Available Benchmarks:"
  echo ""

  if [ ! -d "scenarios" ]; then
    echo "No scenarios directory found."
    exit 0
  fi

  benchmark_count=0

  for benchmark_file in scenarios/*.md; do
    if [ -f "$benchmark_file" ]; then
      benchmark_name=$(basename "$benchmark_file" .md)

      # Skip the TEMPLATE directory
      if [ "$benchmark_name" = "TEMPLATE" ]; then
        continue
      fi

      benchmark_count=$((benchmark_count + 1))

      echo "🎯 $benchmark_name"

      # Extract and show description
      description=$(extract_description "$benchmark_file")
      if [ -n "$description" ]; then
        echo "$description" | fold -s -w 70 | sed 's/^/   /'
      else
        echo "   (no description available)"
      fi

      # Show submission count
      if [ -d "submissions/$benchmark_name" ]; then
        submission_count=$(find "submissions/$benchmark_name" -maxdepth 1 -type d ! -path "submissions/$benchmark_name" | wc -l)
        echo "   📊 $submission_count submissions"
      else
        echo "   📊 0 submissions"
      fi
      echo ""
    fi
  done

  if [ $benchmark_count -eq 0 ]; then
    echo "No benchmarks found."
    echo ""
    echo "💡 Create a new benchmark with: cape benchmark new <benchmark-name>"
  else
    echo "📈 Total: $benchmark_count benchmarks available"
    echo ""
    echo "🔍 Use 'cape benchmark <benchmark-name>' for detailed information"
    echo "💡 Use 'cape benchmark new <benchmark-name>' to create a new benchmark"
    echo "📝 Use 'cape submission new <benchmark> <compiler> <version> <contributor>' to submit"
  fi
fi
