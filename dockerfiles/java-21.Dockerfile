FROM maven:3.9.5-eclipse-temurin-21-alpine

COPY pom.xml /app/pom.xml

WORKDIR /app

# Download dependencies
RUN mvn -B package -Ddir=/tmp/codecrafters-build-redis-java

# Cache Dependencies
RUN mkdir -p /app-cached
RUN mv /app/target /app-cached # Is this needed?

# Pre-compile steps
RUN printf "cd \${CODECRAFTERS_REPOSITORY_DIR} && mvn -B package -Ddir=/tmp/codecrafters-build-redis-java && sed -i 's/^\(mvn .*\)/#\1/' ./spawn_redis_server.sh && sed -i 's|/tmp/codecrafters-redis-target|/tmp/codecrafters-build-redis-java|g' ./spawn_redis_server.sh" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh