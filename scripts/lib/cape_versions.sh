#!/usr/bin/env bash
# CAPE version configuration for dual-CEK (current + preview) report filtering
#
# Current: production plutus-core version (matches cardano-node on mainnet).
#   1.63.0.0 is the line shipped by cardano-node 11.0.1, the release running
#   mainnet since the van Rossem hard fork (protocol v11, 2026-07-18). This is
#   the only load-bearing constant here: cape_is_preview_submission compares
#   every submission's min_plutus_version against it.
# Preview: latest tagged plutus release, pinned in cabal.project.preview.
#   Informational only -- nothing reads it. measure --preview deliberately runs
#   every above-threshold submission against the single preview binary,
#   whatever version each one declares.

CAPE_CURRENT_PLUTUS_VERSION="1.63.0.0"
CAPE_PREVIEW_PLUTUS_VERSION="1.68.0.0"

# Compare two semver4 versions using sort -V
# Returns 0 (true) if $1 > $2
cape_version_gt() {
  [ "$1" != "$2" ] && [ "$(printf '%s\n%s' "$1" "$2" | sort -V | head -n1)" = "$2" ]
}

# Determine if a submission belongs to the preview track
# Usage: cape_is_preview_submission <min_plutus_version>
# Returns 0 (true) if the submission requires a version newer than current
cape_is_preview_submission() {
  local min_version="${1:-}"
  if [ -z "$min_version" ]; then
    return 1  # No version specified = current track
  fi
  cape_version_gt "$min_version" "$CAPE_CURRENT_PLUTUS_VERSION"
}
