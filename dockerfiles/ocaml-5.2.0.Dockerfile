# syntax=docker/dockerfile:1.7-labs
FROM ocaml/opam:alpine-3.20-ocaml-5.2

# The image uses opam as the user, so let's set OPAMROOT to re-use whatever is already built
ENV OPAMROOT /home/opam/.opam

# Ensures the container is re-built if dune/dune-project changes
ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="dune,dune-project"

# Change to root user. All other images seem to use root, so let's do the same here
# hadolint ignore=DL3002
USER root

# Install dune
RUN opam update && opam install dune.3.16.0 --yes

# Dune path is /home/opam/.opam/5.2/bin/dune
ENV PATH="${OPAMROOT}/5.2/bin:${PATH}"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Cache dependencies
RUN opam install . --yes

# This runs dune build
RUN .codecrafters/compile.sh
