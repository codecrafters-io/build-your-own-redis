# syntax=docker/dockerfile:1.7-labs
FROM swift:6.0-focal

# Ensures the container is re-built if Package.swift changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Package.swift"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Runs swift build, which caches dependencies
RUN .codecrafters/compile.sh