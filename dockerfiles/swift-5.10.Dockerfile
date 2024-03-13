FROM swift:5.10

COPY app/Package.swift /app/Package.swift

WORKDIR /app
RUN swift build

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && swift build --package-path app/" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
