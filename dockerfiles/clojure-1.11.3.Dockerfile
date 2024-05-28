FROM clojure:openjdk-19-lein-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="project.clj"

WORKDIR /app
COPY project.clj ./

RUN lein deps

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && lein deps && lein uberjar" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
