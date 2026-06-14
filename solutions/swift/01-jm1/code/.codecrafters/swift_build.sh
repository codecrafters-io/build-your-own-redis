#!/bin/sh
#
# Build the package with a pinned CPU count.
#
# SwiftPM bakes the host's CPU count into every whole-module Swift compile
# command as `-num-threads`/`-j`. The CodeCrafters image is built on a host with
# more cores than the test runner, so the command lines—and therefore llbuild's
# command signatures—differ between build and run, forcing a full rebuild of the
# whole module graph. LD_PRELOAD-ing a small shim (cpushim.c) makes SwiftPM read
# a constant CPU count, so the commands (and signatures) are identical in both
# places and runs stay incremental.
#
# This is invoked by BOTH compile.sh and the Dockerfile's dependency pre-build,
# so they emit the same commands, and the build.db that ships matches what the
# runner regenerates. See cpushim.c for why intercepting sysconf is the only
# way to pin -num-threads.

set -e

# Pinned CPU count. Any constant keeps incremental builds deterministic across
# different-core hosts; this value only affects build parallelism. 4 CPUs avoids
# oversubscription on the test runners. If you need to speed up the infrequent
# dependency/image builds, you can raise it toward the image-build host's core
# count. Test runs remain incremental either way.
NCPUS=4

dir="$(dirname "$0")"
SHIM=/tmp/codecrafters-cpushim.so
# If this fails for any reason, fall back to a shim-less build (which is still
# correct, it just won't have the incremental build optimisation across
# different-core hosts).
clang -shared -fPIC -o "$SHIM" "$dir/cpushim.c" -ldl 2>/dev/null || SHIM=""

LD_PRELOAD="$SHIM" FAKE_NPROC="$NCPUS" \
    swift build -c release --jobs "$NCPUS" --build-path /tmp/codecrafters-build-redis-swift
