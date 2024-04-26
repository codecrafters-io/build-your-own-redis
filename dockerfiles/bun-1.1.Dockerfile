FROM oven/bun:1.1.4-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="package.json,bun.lockb"

WORKDIR /app

COPY package.json ./
COPY bun.lockb ./

# For reproducible builds.
# This will install the exact versions of each package specified in the lockfile.
# If package.json disagrees with bun.lockb, Bun will exit with an error. The lockfile will not be updated.
RUN bun install --frozen-lockfile

RUN mkdir -p /app-cached
# If the node_modules directory exists, move it to /app-cached
RUN if [ -d "/app/node_modules" ]; then mv /app/node_modules /app-cached; fi
