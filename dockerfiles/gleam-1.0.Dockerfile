# syntax=docker/dockerfile:1.7-labs
FROM ghcr.io/gleam-lang/gleam:v1.0.0-erlang-alpine

# Rebuild if gleam.toml or manifest.toml change
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="gleam.toml,manifest.toml"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Force deps to be downloaded
RUN gleam build

# Cache build directory
RUN mkdir -p /app-cached
RUN mv build /app-cached/build

# Pre-compile steps (legacy)
RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && gleam build" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

# Once the heavy steps are done, we can copy all files back
COPY . /app