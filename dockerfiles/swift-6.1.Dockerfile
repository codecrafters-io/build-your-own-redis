# syntax=docker/dockerfile:1.7-labs
FROM swift:6.0-focal

# Ensures the container is re-built if Package.swift changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Package.swift,Package.resolved"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN .codecrafters/compile.sh

# Cache dependencies (TODO: Check if this is required, or whether build implicitly caches dependencies)
RUN swift package --build-path /tmp/codecrafters-build-redis-swift resolve