FROM clojure:openjdk-19-lein-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="project.clj"

WORKDIR /app
COPY project.clj ./

RUN lein deps

RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && lein deps && lein uberjar && sed -i 's/^\(lein .*\)/#\1/' ./spawn_redis_server.sh\n" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
