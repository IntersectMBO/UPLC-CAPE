#!/usr/bin/env bash
# Common helpers for CAPE subcommands
# Source this file from subcommands after defining SCRIPT_DIR

# Strict mode is expected in callers

# Command checks
cape_has_cmd() { command -v "$1" > /dev/null 2>&1; }

cape_require_cmd() {
  local cmd="$1"
  local hint="${2:-Run 'nix develop'.}"
  if ! command -v "$cmd" > /dev/null 2>&1; then
    echo "Error: required command '$cmd' not found. ${hint}" >&2
    exit 1
  fi
}

cape_require_cmds() {
  local c
  for c in "$@"; do
    cape_require_cmd "$c"
  done
}

# Script dir and project root
cape_script_dir() {
  # shellcheck disable=SC3057
  cd "$(dirname "${BASH_SOURCE[1]}")" && pwd
}

cape_detect_root() {
  local sd="$1"
  if [[ -z "${PROJECT_ROOT:-}" ]]; then
    PROJECT_ROOT="$(cd "$sd/../../.." && pwd)"
    export PROJECT_ROOT
  fi
}

# Colors and logging (respect NO_COLOR)
# Call this to (re)apply color settings, e.g., after toggling NO_COLOR in a script
cape_apply_color_prefs() {
  if [[ -n "${NO_COLOR:-}" || ! -t 1 ]]; then
    CAPE_RED=""
    CAPE_GREEN=""
    CAPE_YELLOW=""
    CAPE_BLUE=""
    CAPE_NC=""
  else
    CAPE_RED='\033[0;31m'
    CAPE_GREEN='\033[0;32m'
    CAPE_YELLOW='\033[1;33m'
    CAPE_BLUE='\033[0;34m'
    CAPE_NC='\033[0m'
  fi
}
# Initialize colors now
cape_apply_color_prefs

cape_info() { echo -e "${CAPE_BLUE}INFO:${CAPE_NC} $1"; }
cape_warn() { echo -e "${CAPE_YELLOW}WARN:${CAPE_NC} $1" >&2; }
cape_error() { echo -e "${CAPE_RED}ERROR:${CAPE_NC} $1" >&2; }
cape_success() { echo -e "${CAPE_GREEN}SUCCESS:${CAPE_NC} $1"; }
# New: debug logger (enabled when CAPE_VERBOSE=1)
cape_debug() { if [[ "${CAPE_VERBOSE:-0}" -eq 1 ]]; then echo -e "${CAPE_BLUE}DEBUG:${CAPE_NC} $1" >&2; fi; }
cape_die() {
  local code="${2:-1}"
  cape_error "$1"
  exit "$code"
}

# Temp files helpers (shared)
# Register temp files and ensure cleanup with a trap
CAPE_TMP_FILES=()
cape_register_tmp() { [[ -n "$1" ]] && CAPE_TMP_FILES+=("$1"); }
cape_mktemp() {
  local f
  f="$(mktemp)"
  CAPE_TMP_FILES+=("$f")
  printf '%s\n' "$f"
}
# New: create and register a temporary directory
cape_mktemp_dir() {
  local d
  d="$(mktemp -d)"
  CAPE_TMP_FILES+=("$d")
  printf '%s\n' "$d"
}
cape_cleanup_tmpfiles() {
  local f
  for f in "${CAPE_TMP_FILES[@]}"; do
    if [[ -n "$f" && -e "$f" ]]; then
      rm -rf "$f" || true
    fi
  done
}
cape_enable_tmp_cleanup() { trap 'cape_cleanup_tmpfiles' EXIT INT TERM; }

# Help detection
cape_help_requested() {
  local a
  for a in "$@"; do
    case "$a" in
      -h | --help | help) return 0 ;;
    esac
  done
  return 1
}

# Help rendering with gomplate
cape_render_help() {
  local script_path="$1"
  shift || true
  local extra_json="${1:-}"
  cape_require_cmd gomplate
  local script_dir
  script_dir="$(cd "$(dirname "$script_path")" && pwd)"
  local tmpl="$script_dir/$(basename "$script_path" .sh).help.tmpl"
  if [[ ! -f "$tmpl" ]]; then
    echo "Error: help template not found: $tmpl" >&2
    exit 1
  fi
  local tmp_ctx
  tmp_ctx="$(cape_mktemp)"
  printf '{"has_glow": %s}\n' "$(cape_has_cmd glow && echo true || echo false)" > "$tmp_ctx"
  if [[ -n "$extra_json" ]] && cape_has_cmd jq; then
    local merged
    merged="$(jq -s '.[0] * .[1]' "$tmp_ctx" <(printf '%s' "$extra_json"))"
    printf '%s' "$merged" > "$tmp_ctx"
  fi
  # Pass the datasource as JSON to gomplate so fields like .has_glow are available
  local ds
  ds="file://$tmp_ctx?type=application/json"
  gomplate -f "$tmpl" -c .="$ds"
  # best-effort cleanup (also handled by global tmp cleanup if enabled)
  rm -rf "$tmp_ctx" 2> /dev/null || true
}

# Path utility relative to PROJECT_ROOT
cape_relpath() {
  local target="$1"
  if [[ -z "${PROJECT_ROOT:-}" ]]; then
    echo "$target"
    return
  fi
  if [[ "$target" == "$PROJECT_ROOT" ]]; then
    echo "."
  elif [[ "$target" == "$PROJECT_ROOT"/* ]]; then
    echo "${target#$PROJECT_ROOT/}"
  else
    echo "$target"
  fi
}

# Iterator over submission directories (excludes TEMPLATE scenario)
# Usage: cape_each_submission_dir [root] -> prints absolute submission dir paths
cape_each_submission_dir() {
  local root="${1:-$PROJECT_ROOT/submissions}"
  [[ -d "$root" ]] || return 0
  local scenario_dir
  for scenario_dir in "$root"/*/; do
    [[ -d "$scenario_dir" ]] || continue
    local base
    base="$(basename "$scenario_dir")"
    [[ "$base" == "TEMPLATE" ]] && continue
    local submission_dir
    for submission_dir in "$scenario_dir"*/; do
      [[ -d "$submission_dir" ]] || continue
      printf '%s\n' "${submission_dir%/}"
    done
  done
}

# Get the measure binary path or fallback to cabal run
# Returns either a direct binary path or "cabal run measure --"
cape_measure_binary() {
  local binary_path
  # Try to get the binary path from cabal
  if binary_path=$(cd "$PROJECT_ROOT" && cabal list-bin measure 2> /dev/null) && [[ -x "$binary_path" ]]; then
    echo "$binary_path"
  else
    # Fallback to cabal run if binary detection fails
    echo "cabal run measure --"
  fi
}

# Write metrics.json with timestamp preservation
# Merges raw metrics from measure tool with metadata and preserves timestamp if unchanged
# Args:
#   $1: tmp_raw_metrics - Path to temporary file with raw metrics from measure tool
#   $2: output_file - Path to final metrics.json file
#   $3: scenario - Scenario name
#   $4: stdout_file - Path to stdout from measure tool (for extracting evaluator info)
cape_write_metrics_json() {
  local tmp_raw="$1"
  local output_file="$2"
  local scenario="$3"
  local stdout_file="$4"

  cape_require_cmd jq

  # Initialize metadata holders
  local existing_notes="" existing_version="" existing_evaluator=""
  local was_existing=0

  if [[ -f "$output_file" ]]; then
    was_existing=1
    existing_notes=$(jq -r '.notes // empty' "$output_file" 2> /dev/null || echo "") || true
    existing_version=$(jq -r '.version // empty' "$output_file" 2> /dev/null || echo "") || true
    existing_evaluator=$(jq -r '.execution_environment.evaluator // empty' "$output_file" 2> /dev/null || echo "") || true
  fi

  # Set defaults
  if [[ -z "$existing_notes" ]]; then
    existing_notes="Generated using UPLC-CAPE measure tool"
  fi
  if [[ -z "$existing_version" ]]; then
    existing_version="1.0.0"
  fi

  # Extract evaluator from stdout if available
  local evaluator=""
  if [[ -f "$stdout_file" ]]; then
    evaluator=$(grep -E '^Evaluator:' "$stdout_file" | sed -E 's/^Evaluator: *//') || true
  fi
  if [[ -z "$evaluator" ]]; then
    evaluator="$existing_evaluator"
  fi
  if [[ -z "$evaluator" ]]; then
    evaluator="unknown"
  fi

  local timestamp
  timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

  # Preserve existing timestamp if measurements are identical
  if [[ $was_existing -eq 1 ]]; then
    # Create temporary metrics with new measurements for comparison
    local tmp_new_metrics
    tmp_new_metrics="$(cape_mktemp)"
    if jq -n \
      --slurpfile m "$tmp_raw" \
      --arg evaluator "$evaluator" \
      --arg notes "$existing_notes" \
      --arg scenario "$scenario" \
      --arg version "$existing_version" \
      --arg timestamp "$timestamp" '
      $m[0] + {
        execution_environment: {
          evaluator: $evaluator
        },
        notes: $notes,
        scenario: $scenario,
        timestamp: $timestamp,
        version: $version
      }
    ' > "$tmp_new_metrics" 2> /dev/null; then

      # Compare new metrics with existing (excluding timestamp)
      local new_metrics_no_ts existing_metrics_no_ts
      new_metrics_no_ts=$(jq 'del(.timestamp)' "$tmp_new_metrics" 2> /dev/null || echo "{}")
      existing_metrics_no_ts=$(jq 'del(.timestamp)' "$output_file" 2> /dev/null || echo "{}")

      if [[ "$new_metrics_no_ts" == "$existing_metrics_no_ts" ]]; then
        # Preserve original timestamp if measurements are identical
        local existing_timestamp
        existing_timestamp=$(jq -r '.timestamp // empty' "$output_file" 2> /dev/null || echo "")
        if [[ -n "$existing_timestamp" ]]; then
          timestamp="$existing_timestamp"
          cape_debug "Measurements unchanged, preserving timestamp: $existing_timestamp"
        fi
      fi
    fi
    rm -f "$tmp_new_metrics" || true
  fi

  # Create final metrics.json
  local tmp_out
  tmp_out="$(cape_mktemp)"
  if ! jq -n \
    --slurpfile m "$tmp_raw" \
    --arg evaluator "$evaluator" \
    --arg notes "$existing_notes" \
    --arg scenario "$scenario" \
    --arg version "$existing_version" \
    --arg timestamp "$timestamp" '
    $m[0] + {
      execution_environment: {
        evaluator: $evaluator
      },
      notes: $notes,
      scenario: $scenario,
      timestamp: $timestamp,
      version: $version
    }
  ' > "$tmp_out"; then
    cape_error "Failed to compose metrics.json"
    rm -f "$tmp_out" || true
    return 1
  fi

  mv "$tmp_out" "$output_file"
  return 0
}
