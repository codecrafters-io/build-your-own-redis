FROM rust:1.54-buster

COPY Cargo.toml /app/Cargo.toml
COPY Cargo.lock /app/Cargo.lock

RUN mkdir /app/src
RUN echo 'fn main() { println!("Hello World!"); }' > /app/src/main.rs

WORKDIR /app
RUN cargo build --release --target-dir=/tmp/codecrafters-redis-target

RUN rm /tmp/codecrafters-redis-target/release/redis-starter-rust
RUN rm /tmp/codecrafters-redis-target/release/redis-starter-rust.d

RUN find /tmp/codecrafters-redis-target/release -type f -maxdepth 1 -delete
RUN rm -f /tmp/codecrafters-redis-target/release/deps/*redis_starter_rust*
RUN rm -f /tmp/codecrafters-redis-target/release/deps/redis_starter_rust*
RUN rm -f /tmp/codecrafters-redis-target/release/.fingerprint/*redis_starter_rust*
RUN rm -f /tmp/codecrafters-redis-target/release/.fingerprint/redis_starter_rust*

RUN rm -rf /app/src
