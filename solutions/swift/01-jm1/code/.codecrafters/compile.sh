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

swift build -c release --build-path /tmp/codecrafters-build-redis-swift
