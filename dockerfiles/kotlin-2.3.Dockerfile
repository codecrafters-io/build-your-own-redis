# syntax=docker/dockerfile:1.7-labs
FROM gradle:jdk24-alpine

# hadolint ignore=DL3018
RUN apk add --no-cache bash curl unzip && \
    curl -sL "https://github.com/JetBrains/kotlin/releases/download/v2.3.20/kotlin-compiler-2.3.20.zip" -o /tmp/kotlin.zip && \
    unzip -q /tmp/kotlin.zip -d /opt/ && \
    rm /tmp/kotlin.zip
ENV PATH="/opt/kotlinc/bin:${PATH}"

# Ensures the container is re-built if build files change
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="settings.gradle.kts,app/build.gradle.kts,gradle/libs.versions.toml"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Install language-specific dependencies
RUN .codecrafters/compile.sh

RUN mkdir -p /app-cached
RUN if [ -d "/app/.gradle" ]; then mv /app/.gradle /app-cached; fi
