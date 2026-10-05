# syntax=docker/dockerfile:1.7-labs
FROM eclipse-temurin:25-jdk-alpine-3.23

# Pin scala-cli. releases/latest moved to 1.17 on 2026-09-08 and broke CI.
ARG SCALA_CLI_VERSION=1.17.1

SHELL ["/bin/ash", "-eo", "pipefail", "-c"]

# hadolint ignore=DL3018
RUN apk add --no-cache --upgrade bash curl && \
    curl -fsSL "https://github.com/VirtusLab/scala-cli/releases/download/v${SCALA_CLI_VERSION}/scala-cli-x86_64-pc-linux-static.gz" | gzip -d > /usr/local/bin/scala-cli && \
    chmod +x /usr/local/bin/scala-cli

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

WORKDIR /app

RUN .codecrafters/compile.sh