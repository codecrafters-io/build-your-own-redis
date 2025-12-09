# syntax=docker/dockerfile:1.7-labs
FROM ruby:3.4-alpine3.23

# Required for installing the json/async gems
RUN apk add --no-cache \
    build-base~=0.5 \
    libssl3~=3.5 \
    readline-dev~=8.3 \
    zlib-dev~=1.3

# Re-build if Gemfile or Gemfile.lock changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Gemfile,Gemfile.lock"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN bundle install --verbose
