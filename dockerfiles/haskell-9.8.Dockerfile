FROM haskell:9.8.4-bullseye

# Ensures the container is re-built if go.mod or go.sum changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="stack.yaml,package.yaml,stack.yaml.lock"

RUN mkdir -p /etc/stack

# Absence of this causes `stack run` to raise a permissions error
RUN echo "allow-different-user: true" >> /etc/stack/config.yaml

# Force usage of the system GHC installation
RUN echo "install-ghc: false" >> /etc/stack/config.yaml
RUN echo "system-ghc: true" >> /etc/stack/config.yaml

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY . /app/
RUN rm -rf /app/.git /app/README.md

ENV STACK_ROOT=/app/.stack
RUN stack build
RUN stack clean codecrafters-redis

RUN mkdir /app-cached
RUN mv .stack-work /app-cached/.stack-work
RUN mv .stack /app-cached/.stack
RUN rm -rf /app/app
