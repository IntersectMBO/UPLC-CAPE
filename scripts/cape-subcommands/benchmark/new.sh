#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Resolve project root relative to this script (CWD-independent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"

# Create a new benchmark scenario from template
# Usage: cape benchmark new [benchmark-name]

# Check for help flag
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Get benchmark name from argument or prompt user
if [ $# -eq 0 ]; then
  echo "Enter benchmark name (lowercase, hyphens allowed):" >&2
  echo "Examples: fibonacci, two-party-escrow, dao-voting" >&2
  echo -n "> " >&2
  read -r BENCHMARK_NAME
  if [ -z "$BENCHMARK_NAME" ]; then
    echo "Error: Benchmark name cannot be empty"
    exit 1
  fi
elif [ $# -eq 1 ]; then
  BENCHMARK_NAME="$1"
else
  cape_render_help "${BASH_SOURCE[0]}"
  exit 1
fi

# Validate benchmark name format (lowercase, hyphens allowed)
if ! echo "$BENCHMARK_NAME" | grep -qE '^[a-z][a-z0-9-]*[a-z0-9]$|^[a-z]$'; then
  echo "Error: Benchmark name must be lowercase, start with a letter, and can contain hyphens"
  echo "Valid examples: fibonacci, two-party-escrow, dao-voting"
  echo "Invalid examples: TwoParty, two_party, 2escrow, -escrow, escrow-"
  exit 1
fi

# Check if benchmark already exists
if [ -f "$PROJECT_ROOT/scenarios/${BENCHMARK_NAME}.md" ]; then
  echo "Error: Benchmark '${BENCHMARK_NAME}' already exists"
  echo "Available benchmarks:"
  find "$PROJECT_ROOT/scenarios" -maxdepth 1 -type f -name '*.md' -printf '%f\n' | sed -e 's/\\.md$//' -e '/^TEMPLATE$/d' | sed 's/^/  /' || echo "  (none)"
  exit 1
fi

# Create benchmark file from template
echo "Creating new benchmark: $BENCHMARK_NAME"

# Read template and replace placeholders
if [ ! -f "$PROJECT_ROOT/scenarios/TEMPLATE/scenario-template.md" ]; then
  echo "Error: Template not found at scenarios/TEMPLATE/scenario-template.md" >&2
  exit 1
fi
template_content=$(cat "$PROJECT_ROOT/scenarios/TEMPLATE/scenario-template.md")

# Replace placeholders with actual benchmark name
benchmark_content=$(echo "$template_content" | sed "s/{scenario_name}/$BENCHMARK_NAME/g")

# Convert benchmark name to title case for display
benchmark_title=$(echo "$BENCHMARK_NAME" | sed 's/-/ /g' | sed 's/\b\w/\U&/g')
benchmark_content=$(echo "$benchmark_content" | sed "s/{Scenario Name}/$benchmark_title/g")

# Write the new benchmark file
echo "$benchmark_content" > "$PROJECT_ROOT/scenarios/${BENCHMARK_NAME}.md"

echo "✅ Benchmark '${BENCHMARK_NAME}' created successfully!"
echo "📂 File: $PROJECT_ROOT/scenarios/${BENCHMARK_NAME}.md"

echo ""
echo "Next steps:"
echo "1. Edit scenarios/${BENCHMARK_NAME}.md to define your benchmark"
echo "2. Replace all {placeholder} values with actual content"
echo "3. Define the target computation and expected results"
echo "4. Specify measurement guidelines and constraints"
echo "5. Add test cases and validation criteria"
echo ""
echo "Once the benchmark is complete, you can create submissions with:"
echo "  cape submission new ${BENCHMARK_NAME} <compiler> <version> <contributor>"
