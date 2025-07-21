#!/usr/bin/env bash
set -euo pipefail

# Create a new submission folder with standardized naming and structure
# Usage: cape submission new [scenario] [language] [version] [github-handle]

# Check for help flag
if [ $# -eq 1 ] && [ "$1" = "--help" ]; then
  echo "Usage: cape submission new [scenario] [language] [version] [github-handle]"
  echo "Example: cape submission new fibonacci Aiken 1.0.8 Unisay"
  echo "Note: Missing arguments will be prompted interactively"
  echo ""
  echo "Arguments:"
  echo "  scenario      - Name of the benchmark scenario"
  echo "  language      - Compiler/language name (e.g., Aiken, Plinth, Plutarch)"
  echo "  version       - Compiler version (e.g., 1.0.8)"
  echo "  github-handle - Your GitHub username"
  exit 0
fi

# Function to prompt for missing arguments
prompt_for_arg() {
  local arg_name="$1"
  local prompt_text="$2"
  local value=""

  echo -n "$prompt_text " >&2
  echo -n "> " >&2
  read -r value

  if [ -z "$value" ]; then
    echo "Error: $arg_name cannot be empty" >&2
    exit 1
  fi

  echo "$value"
}

# Get arguments from command line or prompt user
if [ $# -eq 0 ]; then
  # No arguments provided - prompt for all
  SCENARIO=$(prompt_for_arg "Scenario" "Enter benchmark/scenario name:")
  LANGUAGE=$(prompt_for_arg "Language" "Enter compiler/language name (e.g., Aiken, Plinth, Plutarch):")
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.0.8):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 1 ]; then
  # One argument provided - prompt for the rest
  SCENARIO="$1"
  LANGUAGE=$(prompt_for_arg "Language" "Enter compiler/language name (e.g., Aiken, Plinth, Plutarch):")
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.0.8):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 2 ]; then
  # Two arguments provided - prompt for the rest
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.0.8):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 3 ]; then
  # Three arguments provided - prompt for the last one
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION="$3"
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 4 ]; then
  # All arguments provided
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION="$3"
  GITHUB_HANDLE="$4"
else
  echo "Usage: cape submission new [scenario] [language] [version] [github-handle]"
  echo "Example: cape submission new fibonacci Aiken 1.0.8 Unisay"
  echo "Note: Missing arguments will be prompted interactively"
  exit 1
fi

# Create standardized folder name
SUBMISSION_FOLDER="${LANGUAGE}_${VERSION}_${GITHUB_HANDLE}"
SUBMISSION_PATH="submissions/${SCENARIO}/${SUBMISSION_FOLDER}"

# Check if scenario exists
if [ ! -f "scenarios/${SCENARIO}.md" ]; then
  echo "Error: Benchmark '${SCENARIO}' not found in scenarios/"
  echo "Available benchmarks:"
  ls scenarios/*.md 2> /dev/null | grep -v "TEMPLATE" | sed 's/scenarios\///;s/\.md$//' | sed 's/^/  /' || echo "  (none)"
  exit 1
fi

# Create submission directory
mkdir -p "$SUBMISSION_PATH"

# Copy template files and customize them
echo "Creating submission folder: $SUBMISSION_PATH"

# Create placeholder UPLC file
cat > "$SUBMISSION_PATH/${SCENARIO}.uplc" << EOF
# TODO: Replace this with your compiled UPLC program
# This should be a fully-applied UPLC script with the benchmark target baked in
# For example, for fibonacci benchmark, it should compute fibonacci(25) = 75025
EOF

# Create metrics.json from template
sed "s/<scenario>/$SCENARIO/g" submissions/TEMPLATE/metrics-template.json > "$SUBMISSION_PATH/metrics.json"

# Create metadata.json from template with language info
sed -e "s/<string>/$LANGUAGE/g" -e "s/<version>/$VERSION/g" submissions/TEMPLATE/metadata-template.json > "$SUBMISSION_PATH/metadata.json"

# Create README.md from template
sed -e "s/<scenario>/$SCENARIO/g" -e "s/<submission-id>/$SUBMISSION_FOLDER/g" submissions/TEMPLATE/benchmark-README-template.md > "$SUBMISSION_PATH/README.md"

# Create optional directories
mkdir -p "$SUBMISSION_PATH/source"
echo "# Optional: Place your source code files here" > "$SUBMISSION_PATH/source/.gitkeep"

# Create optional config.json template
cat > "$SUBMISSION_PATH/config.json" << EOF
{
  "comment": "Optional: Include compilation parameters that affect UPLC output",
  "optimization_flags": [],
  "compiler_settings": {},
  "build_environment": {}
}
EOF

echo "✅ Submission folder initialized successfully!"
echo "📂 Path: $SUBMISSION_PATH"
echo ""
echo "Next steps:"
echo "1. Replace ${SCENARIO}.uplc with your compiled UPLC program"
echo "2. Fill in metrics.json with your performance measurements"
echo "3. Update metadata.json with your compiler details"
echo "4. Edit README.md with implementation notes"
echo "5. (Optional) Add source code to source/ directory"
echo "6. (Optional) Update config.json with compilation parameters"
