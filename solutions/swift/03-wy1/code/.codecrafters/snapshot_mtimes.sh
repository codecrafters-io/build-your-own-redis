#!/usr/bin/env bash

# Snapshot files' mtimes after building, before creating the image.
# 
# This works around a bug where—depending on the container backend—source and
# build files copied into the image may have the nanoseconds truncated off their
# mtimes. This creates an inconsistency, when a container uses the image,
# between the filesystem (no nanoseconds) and what Swift's llbuild recorded
# (includes nanoseconds). This mismatch of timestamps causes llbuild to think
# all files have changed since the last build, causing a full rebuild every time
# (no incremental builds). By snapshotting after the build and before the image
# is created, the mtimes can be restored before the next build in the container
# to match what llbuild recorded, thus avoiding unnecessary rebuilds and
# speeding up the build process.
#
# This script captures the mtimes of all source and build files into a snapshot
# file, which is then included in the image. The companuin `restore_mtimes.sh`
# script can read this snapshot file and restore the mtimes before the next
# build.
#
# The format is intentionally kept simple for easy parsing in the restore script.
# Each record is:
#   path NUL seconds NUL nanoseconds NUL
# where the timestamp is split into seconds and nanoseconds, to avoid issues
# with floating point precision and locale-specific decimal separators.

set -euo pipefail

# Use a consistent locale and timezone to ensure consistent timestamp formatting.
export LC_ALL=C
export TZ=UTC

OUT="${1:-/tmp/codecrafters-mtimes.snapshot}"

ROOTS=(
  "/app"
  "/tmp/codecrafters-build-redis-swift"
)

tmp_out="${OUT}.tmp"
# Truncate the output file at the start. This ensures we don't accidentally
# append to an old snapshot if something goes wrong during this script.
: > "$tmp_out"

extract_nsec_from_stat_y() {
  # Input example from GNU stat %y:
  #   2026-04-26 22:40:35.123456789 +0000
  #
  # Output:
  #   123456789
  local stat_y="$1"
  local frac

  # Extract the nanoseconds part using a regex. Otherwise, use 0.
  if [[ "$stat_y" =~ :[0-9]{2}\.([0-9]+)[[:space:]] ]]; then
    frac="${BASH_REMATCH[1]}"
  else
    frac="0"
  fi

  # Right-pad/truncate to exactly 9 digits, so this is a nanosecond field.
  frac="${frac}000000000"
  printf '%s\n' "${frac:0:9}"
}

save_one_path() {
  local path="$1"
  local seconds
  local stat_y
  local nanoseconds

  # GNU stat default behavior is suitable here: it reports the path itself,
  # not the symlink target, unless -L is used.
  seconds="$(stat -c '%Y' -- "$path")"
  stat_y="$(stat -c '%y' -- "$path")"
  nanoseconds="$(extract_nsec_from_stat_y "$stat_y")"

  # Record format:
  #   path NUL seconds NUL nanoseconds NUL
  printf '%s\0%s\0%s\0' "$path" "$seconds" "$nanoseconds" >> "$tmp_out"
}

for root in "${ROOTS[@]}"; do
  if [[ ! -e "$root" && ! -L "$root" ]]; then
    continue
  fi

  # -P means do not follow symlinks.
  # -print0 includes hidden files automatically and is safe for unusual paths.
  while IFS= read -r -d '' path; do
    save_one_path "$path"
  done < <(find -P "$root" -print0)
done

mv "$tmp_out" "$OUT"

printf 'Created mtimes snapshot at %s\n' "$OUT"
