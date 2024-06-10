# syntax=docker/dockerfile:1.7-labs
FROM rust:1.77-buster

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN cargo build --release --target-dir=/tmp/codecrafters-redis-target

RUN rm /tmp/codecrafters-redis-target/release/redis-starter-rust
RUN rm /tmp/codecrafters-redis-target/release/redis-starter-rust.d

# Build caching schenanigans, see if this can be simplified
RUN find /tmp/codecrafters-redis-target/release -type f -maxdepth 1 -delete
RUN rm -f /tmp/codecrafters-redis-target/release/deps/*redis_starter_rust*
RUN rm -f /tmp/codecrafters-redis-target/release/deps/redis_starter_rust*
RUN rm -f /tmp/codecrafters-redis-target/release/.fingerprint/*redis_starter_rust*
RUN rm -f /tmp/codecrafters-redis-target/release/.fingerprint/redis_starter_rust*

RUN rm -rf /app/src

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && cargo build --release --target-dir=/tmp/codecrafters-redis-target --manifest-path Cargo.toml" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Cargo.toml,Cargo.lock"

# Once the heavy steps are done, we can copy all files back
COPY . /app