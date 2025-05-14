# syntax=docker/dockerfile:1.7-labs
FROM rust:1.86-bookworm

# Rebuild the container if these files change
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Cargo.toml,Cargo.lock"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# This runs cargo build
RUN .codecrafters/compile.sh
