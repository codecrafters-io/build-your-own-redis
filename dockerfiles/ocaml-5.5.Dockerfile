# syntax=docker/dockerfile:1.7-labs
FROM ocaml/opam:debian-13-ocaml-5.5

# The image uses opam as the user, so let's set OPAMROOT to re-use whatever is already built
ENV OPAMROOT /home/opam/.opam

# Ensures the container is re-built if dune/dune-project changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="dune,dune-project,codecrafters_redis.opam"

# Change to root user. All other images seem to use root, so let's do the same here
# hadolint ignore=DL3002,DL3066
USER root

# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
        libncurses-dev \
        libreadline-dev \
        pkg-config \
        xz-utils && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install dune
RUN opam install dune.3.21.0 --yes

# Dune path is /home/opam/.opam/5.5/bin/dune
ENV PATH="${OPAMROOT}/5.5/bin:${PATH}"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Cache dependencies
RUN opam install . --yes
