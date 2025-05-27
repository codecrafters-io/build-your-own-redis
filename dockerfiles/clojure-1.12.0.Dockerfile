FROM clojure:temurin-24-lein-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="project.clj"

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY project.clj /app

RUN lein deps
RUN lein uberjar
