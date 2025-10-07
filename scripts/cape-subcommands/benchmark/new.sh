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

# Get benchmark name from argument (required)
if [ $# -eq 0 ]; then
  echo "Error: Benchmark name is required" >&2
  echo "Usage: cape benchmark new <name>" >&2
  echo "Examples: fibonacci, two-party-escrow, dao-voting" >&2
  exit 1
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
if [ -d "$PROJECT_ROOT/scenarios/${BENCHMARK_NAME}" ]; then
  echo "Error: Benchmark '${BENCHMARK_NAME}' already exists"
  echo "Available benchmarks:"
  find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d ! -name TEMPLATE -exec basename {} \; | sed 's/^/  /' || echo "  (none)"
  exit 1
fi

# Create benchmark directory from template
echo "Creating new benchmark: $BENCHMARK_NAME"

# Copy template directory
if [ ! -d "$PROJECT_ROOT/scenarios/TEMPLATE" ]; then
  echo "Error: Template directory not found at scenarios/TEMPLATE/" >&2
  exit 1
fi

# Create benchmark directory
mkdir -p "$PROJECT_ROOT/scenarios/${BENCHMARK_NAME}"

# Copy template files and replace placeholders
for template_file in "$PROJECT_ROOT/scenarios/TEMPLATE"/*; do
  if [ -f "$template_file" ]; then
    filename=$(basename "$template_file")
    # Replace template filename if needed
    target_filename="$filename"
    if [[ "$filename" == *"template"* ]]; then
      target_filename=$(echo "$filename" | sed "s/template/${BENCHMARK_NAME}/g")
    fi

    # Read template and replace placeholders
    template_content=$(cat "$template_file")
    benchmark_content=$(echo "$template_content" | sed "s/{scenario_name}/$BENCHMARK_NAME/g")

    # Convert benchmark name to title case for display
    benchmark_title=$(echo "$BENCHMARK_NAME" | sed 's/-/ /g' | sed 's/\b\w/\U&/g')
    benchmark_content=$(echo "$benchmark_content" | sed "s/{Scenario Name}/$benchmark_title/g")

    # Write the new benchmark file
    echo "$benchmark_content" > "$PROJECT_ROOT/scenarios/${BENCHMARK_NAME}/${target_filename}"
  fi
done

echo "✅ Benchmark '${BENCHMARK_NAME}' created successfully!"
echo "📂 Directory: $PROJECT_ROOT/scenarios/${BENCHMARK_NAME}/"

echo ""
echo "Next steps:"
echo "1. Edit files in scenarios/${BENCHMARK_NAME}/ to define your benchmark"
echo "2. Replace all {placeholder} values with actual content"
echo "3. Define the target computation and expected results"
echo "4. Specify measurement guidelines and constraints"
echo "5. Add test cases and validation criteria"
echo ""
echo "Once the benchmark is complete, you can create submissions with:"
echo "  cape submission new ${BENCHMARK_NAME} <compiler> <version> <contributor>"
