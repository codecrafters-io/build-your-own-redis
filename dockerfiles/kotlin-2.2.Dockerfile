# syntax=docker/dockerfile:1.7-labs
FROM gradle:jdk24-alpine

# Ensures the container is re-built if pom.xml changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="settings.gradle.kts,app/build.gradle.kts,gradle/libs.versions.toml"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Install language-specific dependencies
RUN .codecrafters/compile.sh

RUN mkdir -p /app-cached
RUN if [ -d "/app/.gradle" ]; then mv /app/.gradle /app-cached; fi
