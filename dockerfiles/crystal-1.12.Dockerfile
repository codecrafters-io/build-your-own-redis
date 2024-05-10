FROM crystallang/crystal:1.12-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="shard.yml, shard.lock"

WORKDIR /app
COPY shard.yml shard.lock ./

RUN shards install --frozen

RUN mkdir -p /app-cached/
RUN if [ -d "/app/lib" ]; then mv /app/lib /app-cached/lib; fi

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && crystal build -p -s -t -o server app/main.cr && sed -i '/^crystal/ s/^/# /' ./spawn_redis_server.sh" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
