# syntax=docker/dockerfile:1.7-labs
FROM haskell:9.4.6-buster

WORKDIR /app

RUN mkdir -p /etc/stack

# Absence of this causes `stack run` to raise a permissions error
RUN echo "allow-different-user: true" >> /etc/stack/config.yaml

# Force usage of the system GHC installation
RUN echo "install-ghc: false" >> /etc/stack/config.yaml
RUN echo "system-ghc: true" >> /etc/stack/config.yaml

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app


ENV STACK_ROOT=/app/.stack

RUN stack build
RUN stack clean hs-redis-clone
RUN mkdir /app-cached
RUN mv .stack-work /app-cached/.stack-work
RUN mv .stack /app-cached/.stack

RUN rm -rf /app/app

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && stack build" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="stack.yaml,package.yaml,stack.yaml.lock"

# Once the heave steps are done, we can copy all files back
COPY . /app
