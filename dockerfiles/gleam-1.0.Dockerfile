FROM ghcr.io/gleam-lang/gleam:v1.0.0-erlang-alpine

# Pre-compile steps
RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && gleam build" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
