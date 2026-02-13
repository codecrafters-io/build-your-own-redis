# syntax=docker/dockerfile:1.7-labs
FROM ghcr.io/gleam-lang/gleam:v1.14.0-erlang-alpine

# Rebuild if gleam.toml or manifest.toml change
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="gleam.toml,manifest.toml"

RUN apk add --no-cache \
    build-base~=0.5

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Force deps to be downloaded
RUN gleam build

# Cache build directory
RUN mkdir -p /app-cached
RUN mv build /app-cached/build
