# syntax=docker/dockerfile:1.7-labs
FROM node:21.7-alpine3.19

# Ensures the container is re-built if dependency files change
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="package.json,package-lock.json"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Install deps
RUN npm ci

# If the node_modules directory exists, move it to /app-cached
RUN mkdir -p /app-cached
RUN if [ -d "/app/node_modules" ]; then mv /app/node_modules /app-cached; fi