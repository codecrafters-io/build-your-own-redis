FROM maven:3.9.5-eclipse-temurin-8-alpine

COPY pom.xml /app/pom.xml

WORKDIR /app

# Download the dependencies
RUN mvn -B package -Ddir=/tmp/codecrafters-redis-target

# Cache Dependencies
RUN mkdir -p /app-cached
RUN mv /app/target /app-cached # Is this needed?

# Pre-compile steps
RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && mvn -B package -Ddir=/tmp/codecrafters-redis-target && sed -i 's/^\(mvn .*\)/#\1/' ./spawn_redis_server.sh" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh