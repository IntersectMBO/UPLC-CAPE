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
