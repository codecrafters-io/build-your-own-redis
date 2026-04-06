# syntax=docker/dockerfile:1.7-labs
FROM eclipse-temurin:25-jdk-alpine-3.23

SHELL ["/bin/ash", "-eo", "pipefail", "-c"]

RUN apk add --no-cache --upgrade bash=5.3.3-r1 && \
    curl -fsSL https://github.com/VirtusLab/scala-cli/releases/latest/download/scala-cli-x86_64-pc-linux-static.gz | gzip -d > /usr/local/bin/scala-cli && \
    chmod +x /usr/local/bin/scala-cli

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

WORKDIR /app

RUN .codecrafters/compile.sh