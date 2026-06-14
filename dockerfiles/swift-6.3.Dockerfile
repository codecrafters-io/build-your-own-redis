# syntax=docker/dockerfile:1.7-labs
FROM swift:6.3-noble

# Ensures the container is re-built if Package.swift changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Package.swift,Package.resolved"

WORKDIR /app

# Copy only the dependency manifests and build scripts first, so the expensive
# dependency-build layer keyed on these files alone. Source-only changes then
# reuse the cached layer instead of recompiling all dependencies.
COPY Package.* /app/
COPY .codecrafters /app/.codecrafters

# Pre-build the dependencies (swift-nio etc.) against a stub source file. If
# the stub doesn't match the package's target layout the build fails, which is
# fine: compile.sh below will then do the full build instead.
#
# Use the same swift_build.sh as compile.sh (pinned CPU count), so this build
# and the per-submit build emit identical compile commands. Without this,
# Swift PM would re-derive the whole graph at a different core count, thereby
# preventing incremental builds. See swift_build.sh / cpushim.c.
RUN (mkdir -p Sources \
    && echo '// stub' > Sources/main.swift \
    && .codecrafters/swift_build.sh) \
    || true
RUN rm -rf Sources

# Snapshot mtimes so compile.sh's restore step can undo any mtime truncation
# introduced when this layer is exported/restored (see snapshot_mtimes.sh).
RUN .codecrafters/snapshot_mtimes.sh

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# This also refreshes the mtimes snapshot after building, so no separate
# snapshot step is needed here.
RUN .codecrafters/compile.sh
