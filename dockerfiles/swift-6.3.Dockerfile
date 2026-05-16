# syntax=docker/dockerfile:1.7-labs
FROM swift:6.3-noble

# Ensures the container is re-built if Package.swift changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Package.swift,Package.resolved"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN .codecrafters/compile.sh

# Snapshot the mtimes of all source files after a successful build. Their mtimes
# will be restored before the next build in the container. This works around
# some container backends that truncate mtimes, causing Swift's llbuild to
# rebuild everything (no incremental builds).
#
# See the documentation in `snapshot_mtimes.sh` for more details.
RUN .codecrafters/snapshot_mtimes.sh
