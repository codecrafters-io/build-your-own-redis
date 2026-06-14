#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

# Restore the mtimes of all source files before building. This works around some
# container backends that truncate mtimes, causing Swift's llbuild to rebuild
# everything (no incremental builds).
#
# See the documentation in `restore_mtimes.sh` for more details.
#
# If the snapshot restore failed, skip the restore step and proceed with the
# build anyway. This can happen if the snapshot file is missing (e.g. local
# development).
.codecrafters/restore_mtimes.sh || true

# Build with a pinned CPU count so the Swift compile commands (and therefore
# llbuild's command signatures) are identical between the image-build host and
# the test runner, which have different core counts. Without this, every run
# rebuilds the whole module graph. See swift_build.sh and cpushim.c.
.codecrafters/swift_build.sh

# Snapshot the mtimes of all source and build files after a successful build.
# The next build—possibly in a new container created from an image of this
# one—restores them so it stays incremental.
#
# If the snapshot fails, proceed anyway: the worst case is a slower (but still
# correct) next build.
#
# See the documentation in `snapshot_mtimes.sh` for more details.
.codecrafters/snapshot_mtimes.sh || true
