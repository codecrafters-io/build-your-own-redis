FROM alpine:3.19

# Add the testing repository
RUN echo "@community http://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories

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

# Update the package list and install Zig
RUN apk add --no-cache zig@community=0.12.0-r0

RUN printf "set -e \ncd \${CODECRAFTERS_SUBMISSION_DIR} \necho 'Running zig build' \nzig build \necho 'zig build completed.' \n" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
