FROM swift:5.10

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="Package.swift"

COPY app/Package.swift /app/Package.swift
RUN mkdir -p /app/app/Sources
RUN echo "print(\"Hello, world!\")" > /app/app/Sources/main.swift

WORKDIR /app
RUN swift build --package-path app/

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && swift build --package-path app/" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
