FROM debian:stable-slim

# Update package list and install required packages
RUN apt-get update && \
    apt-get install -y curl xz-utils build-essential libc-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set Zig version
ENV ZIG_VERSION=0.12.0

WORKDIR /app

# Download and install Zig to /usr/local/bin
RUN curl -LO https://ziglang.org/download/${ZIG_VERSION}/zig-linux-$(uname -m)-${ZIG_VERSION}.tar.xz && \
    tar -xf zig-linux-$(uname -m)-${ZIG_VERSION}.tar.xz && \
    mkdir -p /usr/local/zig && \
    mv zig-linux-$(uname -m)-${ZIG_VERSION}/* /usr/local/zig/ && \
    rmdir zig-linux-$(uname -m)-${ZIG_VERSION} && \
    rm zig-linux-$(uname -m)-${ZIG_VERSION}.tar.xz

ENV PATH="/usr/local/zig:${PATH}"

# We should use build.zig & build.zig.zon to cache downloading deps, but we don't have this wired up yet.
#
# For anyone interesting in helping out here, this'll need to:
#
# (a) Add build.zig and build.zig.zon to CODECRAFTERS_DEPENDENCY_FILE_PATHS so that the Docker container
#     is re-built when these files change.
# (b) Add step(s) to copy only these files (and not the entire submission) into the Docker container.
# (c) Add a step to force Zig to download dependencies and cache them (they'll then be used in the precompile script).
#
# ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="build.zig,build.zig.zon"

# Create precompile script
RUN printf "set -e \ncd \${CODECRAFTERS_SUBMISSION_DIR} \necho 'Running zig build' \nzig build -Doptimize=ReleaseFast \necho 'zig build completed.' \n" > /codecrafters-precompile.sh

# Make precompile script executable
RUN chmod +x /codecrafters-precompile.sh
