#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# Resolve project root relative to this script (CWD-independent)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/../../lib/cape_common.sh"
cape_detect_root "$SCRIPT_DIR"

# Create a new submission folder with standardized naming and structure
# Usage: cape submission new [scenario] [language] [version] [github-handle]

# Early help
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Parse options (-h | --help)
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
if [ "${1:-}" = "--help" ] || [ "${1:-}" = "help" ]; then
  show_help
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

# Validators
is_valid_language() { [[ $1 =~ ^[A-Za-z][A-Za-z0-9-]*$ ]]; }
is_valid_version() { [[ $1 =~ ^[0-9]+(\.[0-9]+){0,3}([.-][A-Za-z0-9]+)*$ ]]; }
# GitHub: 1-39 chars, alnum or hyphen, cannot start/end with hyphen
is_valid_handle() { [[ $1 =~ ^[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?$ ]] && [[ ${#1} -le 39 ]]; }

# Get arguments from command line or prompt user
if [ $# -eq 0 ]; then
  SCENARIO=$(prompt_for_arg "Scenario" "Enter benchmark/scenario name:")
  LANGUAGE=$(prompt_for_arg "Language" "Enter compiler/language name (e.g., Aiken, Plinth, Plutarch):")
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.1.17 or 1.49.0.0):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 1 ]; then
  SCENARIO="$1"
  LANGUAGE=$(prompt_for_arg "Language" "Enter compiler/language name (e.g., Aiken, Plinth, Plutarch):")
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.1.17 or 1.49.0.0):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 2 ]; then
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.1.17 or 1.49.0.0):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 3 ]; then
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION="$3"
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 4 ]; then
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION="$3"
  GITHUB_HANDLE="$4"
else
  show_help
  exit 1
fi

# Basic input validation
if [[ "$SCENARIO" =~ [[:space:]]|/ ]]; then
  echo "Error: Scenario name must not contain spaces or slashes" >&2
  exit 1
fi
if ! is_valid_language "$LANGUAGE"; then
  echo "Error: Invalid language '$LANGUAGE'. Use letters, digits and hyphens only." >&2
  exit 1
fi
if ! is_valid_version "$VERSION"; then
  echo "Error: Invalid version '$VERSION'. Use digits with dots (and optional pre-release/build)." >&2
  exit 1
fi
if ! is_valid_handle "$GITHUB_HANDLE"; then
  echo "Error: Invalid GitHub handle '$GITHUB_HANDLE'." >&2
  echo "Rules: 1-39 chars, alphanumeric or hyphen, cannot start/end with hyphen." >&2
  exit 1
fi

# Create standardized folder name
SUBMISSION_FOLDER="${LANGUAGE}_${VERSION}_${GITHUB_HANDLE}"
SUBMISSION_PATH="$PROJECT_ROOT/submissions/${SCENARIO}/${SUBMISSION_FOLDER}"

# Check if scenario exists (primary file only)
SCENARIO_FILE="$PROJECT_ROOT/scenarios/${SCENARIO}.md"
if [ ! -f "$SCENARIO_FILE" ]; then
  echo "Error: Benchmark '${SCENARIO}' not found in scenarios/" >&2
  echo "Available benchmarks:" >&2
  find "$PROJECT_ROOT/scenarios" -maxdepth 1 -type f -name '*.md' \
    -printf '%f\n' | sed -e 's/\.md$//' -e '/^TEMPLATE$/d' | sed 's/^/  /' >&2 || echo "  (none)" >&2
  exit 1
fi

# Prevent accidental overwrite
if [ -e "$SUBMISSION_PATH" ]; then
  echo "Error: Submission path already exists: $SUBMISSION_PATH" >&2
  exit 1
fi

# Create submission directory
mkdir -p "$SUBMISSION_PATH"

# Copy template files and customize them
echo "Creating submission folder: $SUBMISSION_PATH"

# Create placeholder UPLC file
cat > "$SUBMISSION_PATH/${SCENARIO}.uplc" << 'EOF'
# TODO: Replace this with your compiled UPLC program
# This should be a fully-applied UPLC script with the benchmark target baked in
# For example, for fibonacci benchmark, it should compute fibonacci(25) = 75025
EOF

# Create metrics.json from template
sed "s/<scenario>/$SCENARIO/g" \
  "$PROJECT_ROOT/submissions/TEMPLATE/metrics-template.json" > "$SUBMISSION_PATH/metrics.json"

# Create metadata.json from template with language info
sed -e "s/<string>/$LANGUAGE/g" -e "s/<version>/$VERSION/g" \
  "$PROJECT_ROOT/submissions/TEMPLATE/metadata-template.json" > "$SUBMISSION_PATH/metadata.json"

# Create README.md from template
sed -e "s/<scenario>/$SCENARIO/g" -e "s/<submission-id>/$SUBMISSION_FOLDER/g" \
  "$PROJECT_ROOT/submissions/TEMPLATE/benchmark-README-template.md" > "$SUBMISSION_PATH/README.md"

# Create optional directories
mkdir -p "$SUBMISSION_PATH/source"
echo "# Optional: Place your source code files here" > "$SUBMISSION_PATH/source/.gitkeep"

# Create optional config.json template
cat > "$SUBMISSION_PATH/config.json" << 'EOF'
{
  "comment": "Optional: Include compilation parameters that affect UPLC output",
  "optimization_flags": [],
  "compiler_settings": {},
  "build_environment": {}
}
EOF

echo "✅ Submission folder initialized successfully!"
echo "📂 Path: $SUBMISSION_PATH"
echo
echo "Next steps:"
echo "1. Replace ${SCENARIO}.uplc with your compiled UPLC program"
echo "2. Fill in metrics.json with your performance measurements"
echo "3. Update metadata.json with your compiler details"
echo "4. Edit README.md with implementation notes"
echo "5. (Optional) Add source code to source/ directory"
echo "6. (Optional) Update config.json with compilation parameters"
