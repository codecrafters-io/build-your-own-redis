# syntax=docker/dockerfile:1.7-labs
FROM clojure:openjdk-18-lein-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="project.clj"

WORKDIR /app
# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN lein deps

RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && lein deps && lein uberjar && sed -i 's/^\(lein .*\)/#\1/' ./spawn_redis_server.sh\n" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

# Once the heave steps are done, we can copy all files back
COPY . /app
