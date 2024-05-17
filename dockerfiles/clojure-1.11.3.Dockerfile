FROM clojure:openjdk-19-lein-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="project.clj"

WORKDIR /app
COPY project.clj ./

RUN lein deps

RUN mkdir -p /app-cached/deps/
RUN if [ -d "/app/deps/" ]; then mv /app/deps/ /app-cached/deps/; fi

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && lein deps" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
