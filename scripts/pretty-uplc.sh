#!/usr/bin/env bash
set -Eeuo pipefail

# Pretty-print UPLC files in place using the plutus executable
# Usage: pretty-uplc.sh <path-to-uplc-file>

if [ $# -eq 0 ]; then
  echo "Usage: pretty-uplc.sh <path-to-uplc-file>" >&2
  exit 1
fi

uplc_file="$1"

if [ ! -f "$uplc_file" ]; then
  echo "Error: File not found: $uplc_file" >&2
  exit 1
fi

# Check if plutus command is available
if ! command -v plutus > /dev/null 2>&1; then
  echo "Error: plutus command not found. Make sure you're in the nix shell." >&2
  exit 1
fi

# Create temporary file
temp_file=$(mktemp) || {
  echo "Error: Failed to create temporary file" >&2
  exit 1
}

# Ensure temp file cleanup on exit
trap 'rm -f "$temp_file"' EXIT

# Pretty-print to temp file, then move to original location
if plutus "$uplc_file" -o "$temp_file"; then
  # Ensure file ends with a newline
  if [ -s "$temp_file" ] && [ "$(tail -c 1 "$temp_file" | wc -l)" -eq 0 ]; then
    echo "" >> "$temp_file"
  fi
  mv "$temp_file" "$uplc_file"
  echo "✓ Pretty-printed: $uplc_file" >&2
else
  echo "Error: plutus command failed for $uplc_file" >&2
  exit 1
fi
