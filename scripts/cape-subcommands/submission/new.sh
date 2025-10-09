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
# Usage: cape submission new [scenario] [language] [version] [github-handle] [variant]

# Early help
if cape_help_requested "$@"; then
  cape_render_help "${BASH_SOURCE[0]}"
  exit 0
fi

# Parse options (-h | --help) and collect positional arguments
OPTIND=1
POSITIONAL_ARGS=()
while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help | help)
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
is_valid_variant() { [[ $1 =~ ^[a-z0-9-]+$ ]]; }

# Get arguments from command line or prompt user
VARIANT=""
if [ $# -eq 0 ]; then
  SCENARIO=$(prompt_for_arg "Scenario" "Enter benchmark/scenario name:")
  LANGUAGE=$(prompt_for_arg "Language" "Enter compiler/language name (e.g., Aiken, Plinth, Plutarch):")
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.1.17 or 1.52.0.0):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 1 ]; then
  SCENARIO="$1"
  LANGUAGE=$(prompt_for_arg "Language" "Enter compiler/language name (e.g., Aiken, Plinth, Plutarch):")
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.1.17 or 1.49.0.0):")
  GITHUB_HANDLE=$(prompt_for_arg "GitHub handle" "Enter your GitHub handle:")
elif [ $# -eq 2 ]; then
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION=$(prompt_for_arg "Version" "Enter compiler version (e.g., 1.1.17 or 1.52.0.0):")
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
elif [ $# -eq 5 ]; then
  SCENARIO="$1"
  LANGUAGE="$2"
  VERSION="$3"
  GITHUB_HANDLE="$4"
  VARIANT="$5"
else
  cape_render_help "${BASH_SOURCE[0]}"
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

# Validate variant if provided
if [ -n "$VARIANT" ]; then
  if ! is_valid_variant "$VARIANT"; then
    echo "Error: Invalid variant '$VARIANT'. Use lowercase letters, digits, and hyphens only." >&2
    exit 1
  fi
fi

# Create standardized folder name
if [ -n "$VARIANT" ]; then
  SUBMISSION_FOLDER="${LANGUAGE}_${VERSION}_${GITHUB_HANDLE}_${VARIANT}"
else
  SUBMISSION_FOLDER="${LANGUAGE}_${VERSION}_${GITHUB_HANDLE}"
fi
SUBMISSION_PATH="$PROJECT_ROOT/submissions/${SCENARIO}/${SUBMISSION_FOLDER}"

# Check if scenario exists (as directory)
SCENARIO_DIR="$PROJECT_ROOT/scenarios/${SCENARIO}"
if [ ! -d "$SCENARIO_DIR" ]; then
  echo "Error: Benchmark '${SCENARIO}' not found in scenarios/" >&2
  echo "Available benchmarks:" >&2
  find "$PROJECT_ROOT/scenarios" -mindepth 1 -maxdepth 1 -type d ! -name TEMPLATE \
    -exec basename {} \; 2> /dev/null | sed 's/^/  /' >&2 || echo "  (none)" >&2
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
sed -e "s/<string>/$LANGUAGE/g" \
  -e "s/<version>/$VERSION/g" \
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
if [ -n "$VARIANT" ]; then
  echo "🏷️  Variant: $VARIANT"
fi
echo
echo "Next steps:"
echo "1. Replace ${SCENARIO}.uplc with your compiled UPLC program"
echo "2. Fill in metrics.json with your performance measurements"
echo "3. Update metadata.json with your compiler details"
echo "4. Edit README.md with implementation notes"
if [ -n "$VARIANT" ]; then
  echo "5. Document optimization approach for variant '$VARIANT'"
fi
echo "$([ -n "$VARIANT" ] && echo '6' || echo '5'). (Optional) Add source code to source/ directory"
echo "$([ -n "$VARIANT" ] && echo '7' || echo '6'). (Optional) Update config.json with compilation parameters"
