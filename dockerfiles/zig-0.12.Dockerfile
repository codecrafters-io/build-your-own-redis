FROM alpine:3.19

# Add the testing repository
RUN echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories

# Update the package list and install Zig
RUN apk add --no-cache zig@testing=0.12.0-r0

RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && zig build-exe ./app/main.zig && sed -i 's/^\(zig .*\)/#\1/' ./your_bittorrent.sh" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
