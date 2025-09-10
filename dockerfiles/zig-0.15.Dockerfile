# syntax=docker/dockerfile:1.7-labs
FROM debian:bookworm

RUN apt-get update && \
    apt-get install --no-install-recommends -y xz-utils=5.4.1-1 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Download and install Zig
RUN curl -O https://ziglang.org/download/0.15.1/zig-x86_64-linux-0.15.1.tar.xz \
    && tar -xf zig-x86_64-linux-0.15.1.tar.xz \
    && mv zig-x86_64-linux-0.15.1 /usr/local/zig \
    && rm zig-x86_64-linux-0.15.1.tar.xz

# Add Zig to PATH
ENV PATH="/usr/local/zig:${PATH}"

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="build.zig,build.zig.zon"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# This runs zig build
RUN .codecrafters/compile.sh

# Cache build directory
RUN mkdir -p /app-cached
RUN mv /app/.zig-cache /app-cached/.zig-cache || true
