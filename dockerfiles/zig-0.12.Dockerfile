FROM alpine:3.19

# Add the testing repository
RUN echo "@community http://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="build.zig.zon"

# Update the package list and install Zig
RUN apk add --no-cache zig@community=0.12.0-r0

RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && zig build" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
