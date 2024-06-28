# syntax=docker/dockerfile:1.7-labs
FROM ruby:3.3-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Gemfile,Gemfile.lock"
WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app
RUN bundle install --verbose
# Once the heave steps are done, we can copy all files back
COPY . /app
