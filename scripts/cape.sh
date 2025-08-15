#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'
trap 'code=$?; echo "Error: ${BASH_SOURCE[0]}:${LINENO}: command \"${BASH_COMMAND}\" failed with exit code ${code}" >&2; exit ${code}' ERR

# CAPE - UPLC-CAPE project management tool
# Usage: cape <command> [subcommand] [args...]

# Handle --project-root argument (used by Nix wrapper)
PROJECT_ROOT_OVERRIDE=""
if [ $# -gt 0 ] && [ "$1" = "--project-root" ]; then
  PROJECT_ROOT_OVERRIDE="$2"
  shift 2
fi

# Global flags: -v/--verbose, --no-color
while [ $# -gt 0 ]; do
  case "$1" in
    -v | --verbose)
      export CAPE_VERBOSE=1
      shift
      ;;
    --no-color)
      export NO_COLOR=1
      shift
      ;;
    --)
      shift
      break
      ;;
    -*)
      # Unknown global option; stop parsing to allow subcommand handling to catch it
      break
      ;;
    *)
      break
      ;;
  esac
done

# Find the project root by looking for the scenarios directory
find_project_root() {
  # If project root is provided via argument (from Nix wrapper), use it
  if [ -n "$PROJECT_ROOT_OVERRIDE" ]; then
    if [ -d "$PROJECT_ROOT_OVERRIDE/scenarios" ] && [ -d "$PROJECT_ROOT_OVERRIDE/scripts" ]; then
      echo "$PROJECT_ROOT_OVERRIDE"
      return 0
    fi
  fi

  # Try to find from script location - this works when running the script directly
  local script_path="${BASH_SOURCE[0]}"

  # Resolve symlinks to get the real script path
  while [ -L "$script_path" ]; do
    script_path="$(readlink "$script_path")"
  done

  local script_dir="$(cd "$(dirname "$script_path")" && pwd)"
  local dir="$script_dir"

  # For the cape.sh script in scripts/ directory, go up to project root
  while [ "$dir" != "/" ]; do
    if [ -d "$dir/scenarios" ] && [ -d "$dir/scripts" ]; then
      echo "$dir"
      return 0
    fi
    dir="$(dirname "$dir")"
  done

  # Fallback: try to find from current working directory
  dir="$PWD"
  while [ "$dir" != "/" ]; do
    if [ -d "$dir/scenarios" ] && [ -d "$dir/scripts" ]; then
      echo "$dir"
      return 0
    fi
    dir="$(dirname "$dir")"
  done

  echo "Error: Could not find project root (looking for scenarios/ and scripts/ directories)" >&2
  echo "Current working directory: $PWD" >&2
  echo "Script location: $script_dir" >&2
  echo "Script path: $script_path" >&2
  echo "Project root override: ${PROJECT_ROOT_OVERRIDE:-'(not set)'}" >&2
  exit 1
}

PROJECT_ROOT="$(find_project_root)"
CAPE_DIR="$PROJECT_ROOT/scripts/cape-subcommands"

# Discover available commands by scanning the filesystem
discover_commands() {
  local commands=()
  if [ -d "$CAPE_DIR" ]; then
    for cmd_dir in "$CAPE_DIR"/*; do
      if [ -d "$cmd_dir" ]; then
        commands+=("$(basename "$cmd_dir")")
      fi
    done
  fi
  printf '%s\n' "${commands[@]}" | sort
}

# Discover subcommands for a given command
discover_subcommands() {
  local command="$1"
  local subcommands=()
  local cmd_dir="$CAPE_DIR/$command"

  if [ -d "$cmd_dir" ]; then
    for script in "$cmd_dir"/*.sh; do
      if [ -f "$script" ]; then
        local basename
        basename="$(basename "$script" .sh)"
        subcommands+=("$basename")
      fi
    done
  fi
  printf '%s\n' "${subcommands[@]}" | sort
}

show_help() {
  local commands=()
  mapfile -t commands < <(discover_commands)

  cat << EOF
CAPE - UPLC-CAPE Project Management Tool

Usage: cape [global flags] <command> [subcommand] [args...]

Global flags:
  -v, --verbose   Enable verbose logs for all subcommands
      --no-color  Disable colored output

EOF

  if [ ${#commands[@]} -gt 0 ]; then
    echo "Available commands:"
    for cmd in "${commands[@]}"; do
      echo "  $cmd"
      local subcommands=()
      mapfile -t subcommands < <(discover_subcommands "$cmd")
      for subcmd in "${subcommands[@]}"; do
        echo "    $subcmd"
      done
    done
    echo ""
    echo "Examples:"
    for cmd in "${commands[@]}"; do
      local subcommands=()
      mapfile -t subcommands < <(discover_subcommands "$cmd")
      if [ ${#subcommands[@]} -gt 0 ]; then
        echo "  cape $cmd ${subcommands[0]} [args...]"
      fi
    done
    echo ""
  else
    echo "No commands found in $CAPE_DIR"
  fi

  echo "For help on specific commands:"
  echo "  cape <command> --help"
}

# Check if any arguments were provided
if [ $# -eq 0 ]; then
  show_help
  exit 1
fi

COMMAND="$1"
shift

case "$COMMAND" in
  "--help" | "-h" | "help")
    show_help
    ;;
  *)
    # Check if command exists
    if [ ! -d "$CAPE_DIR/$COMMAND" ]; then
      echo "Error: Unknown command '$COMMAND'"
      echo ""
      show_help
      exit 1
    fi

    # Handle subcommands
    if [ $# -eq 0 ]; then
      # No subcommand provided - for benchmark command, default to list behavior
      if [ "$COMMAND" = "benchmark" ]; then
        script_path="$CAPE_DIR/$COMMAND/list.sh"
        if [ -f "$script_path" ]; then
          cd "$PROJECT_ROOT"
          exec "$script_path" "$@"
        fi
      # For test command, run the test script directly
      elif [ "$COMMAND" = "test" ]; then
        script_path="$CAPE_DIR/$COMMAND/test.sh"
        if [ -f "$script_path" ]; then
          cd "$PROJECT_ROOT"
          exec "$script_path" "$@"
        fi
      fi

      # For other commands, show available subcommands
      subcommands=()
      mapfile -t subcommands < <(discover_subcommands "$COMMAND")
      echo "Error: $COMMAND command requires a subcommand"
      if [ ${#subcommands[@]} -gt 0 ]; then
        echo "Available subcommands: ${subcommands[*]}"
        echo "Usage: cape $COMMAND <subcommand> [args...]"
      else
        echo "No subcommands found for $COMMAND"
      fi
      exit 1
    fi

    SUBCOMMAND="$1"
    shift

    case "$SUBCOMMAND" in
      "--help" | "-h" | "help")
        # For test command, pass --help through to the test script
        if [ "$COMMAND" = "test" ]; then
          script_path="$CAPE_DIR/$COMMAND/test.sh"
          if [ -f "$script_path" ]; then
            cd "$PROJECT_ROOT"
            exec "$script_path" --help
          fi
        fi

        # Show help for the command
        subcommands=()
        mapfile -t subcommands < <(discover_subcommands "$COMMAND")
        echo "$COMMAND management commands:"
        for subcmd in "${subcommands[@]}"; do
          echo "  $subcmd"
        done
        echo ""
        if [ ${#subcommands[@]} -gt 0 ]; then
          if [ "$COMMAND" = "benchmark" ]; then
            echo "Usage: cape $COMMAND [<subcommand>] [args...]"
            echo "       cape $COMMAND [benchmark-name]"
            echo "Example: cape $COMMAND              # List all benchmarks"
            echo "         cape $COMMAND fibonacci    # Show fibonacci benchmark details"
            echo "         cape $COMMAND list         # Same as first example"
            echo "         cape $COMMAND new <name>   # Create new benchmark"
          elif [ "$COMMAND" = "test" ]; then
            echo "Usage: cape $COMMAND [options]"
            echo "Example: cape $COMMAND              # Run complete test suite"
            echo "         cape $COMMAND --help       # Show test help"
          else
            echo "Usage: cape $COMMAND <subcommand> [args...]"
            echo "Example: cape $COMMAND ${subcommands[0]} [args...]"
          fi
        fi
        ;;
      *)
        # For benchmark command, check if this might be a benchmark name
        if [ "$COMMAND" = "benchmark" ]; then
          # Check if this is a benchmark name instead of a subcommand
          if [ -f "scenarios/$SUBCOMMAND.md" ]; then
            script_path="$CAPE_DIR/$COMMAND/list.sh"
            if [ -f "$script_path" ]; then
              cd "$PROJECT_ROOT"
              exec "$script_path" "$SUBCOMMAND" "$@"
            fi
          fi
        fi

        # Execute the subcommand script
        script_path="$CAPE_DIR/$COMMAND/$SUBCOMMAND.sh"
        if [ ! -f "$script_path" ]; then
          # Special handling for benchmark command - check if it's a benchmark name
          if [ "$COMMAND" = "benchmark" ]; then
            echo "Error: '$SUBCOMMAND' is not a valid benchmark subcommand or benchmark name"
            echo ""
            subcommands=()
            mapfile -t subcommands < <(discover_subcommands "$COMMAND")
            if [ ${#subcommands[@]} -gt 0 ]; then
              echo "Available subcommands: ${subcommands[*]}"
            fi
            echo ""
            echo "Available benchmarks:"
            if [ -d "scenarios" ]; then
              for benchmark_file in scenarios/*.md; do
                if [ -f "$benchmark_file" ]; then
                  benchmark_name=$(basename "$benchmark_file" .md)
                  # Skip the TEMPLATE directory
                  if [ "$benchmark_name" = "TEMPLATE" ]; then
                    continue
                  fi
                  echo "  $benchmark_name"
                fi
              done
            fi
            echo ""
            echo "Usage: cape benchmark [benchmark-name]"
            echo "       cape benchmark list [benchmark-name]"
            echo "       cape benchmark new <benchmark-name>"
          else
            echo "Error: Unknown $COMMAND subcommand '$SUBCOMMAND'"
            subcommands=()
            mapfile -t subcommands < <(discover_subcommands "$COMMAND")
            if [ ${#subcommands[@]} -gt 0 ]; then
              echo "Available subcommands: ${subcommands[*]}"
            fi
          fi
          exit 1
        fi

        # Change to project root before executing subcommand
        cd "$PROJECT_ROOT"
        exec "$script_path" "$@"
        ;;
    esac
    ;;
esac
