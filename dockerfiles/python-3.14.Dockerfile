# syntax=docker/dockerfile:1.7-labs
FROM astral/uv:python3.14-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="pyproject.toml,uv.lock"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Force environment creation
RUN uv sync