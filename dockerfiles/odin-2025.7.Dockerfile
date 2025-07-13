# syntax=docker/dockerfile:1.7-labs
FROM silkeh/clang:19-bookworm

WORKDIR /Odin-install
RUN git clone --depth 1 -b dev-2025-07 https://github.com/odin-lang/Odin.git /Odin-install \
    && git checkout dev-2025-07 \
    && make

RUN mkdir /opt/Odin \
    && cp -R ./base ./core ./shared ./vendor ./odin /opt/Odin/

WORKDIR /
RUN rm -rf /Odin-install
ENV PATH="/opt/Odin:${PATH}"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Install language-specific dependencies
RUN .codecrafters/compile.sh
