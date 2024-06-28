# syntax=docker/dockerfile:1.7-labs
FROM maven:3.9.5-eclipse-temurin-21-alpine


WORKDIR /app
# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# Download the dependencies
RUN mvn -B package -Ddir=/tmp/codecrafters-redis-target

# Cache Dependencies
RUN mkdir -p /app-cached
RUN mv /app/target /app-cached # Is this needed?

# Pre-compile steps
RUN printf "cd \${CODECRAFTERS_SUBMISSION_DIR} && mvn -B package -Ddir=/tmp/codecrafters-redis-target && sed -i 's/^\(mvn .*\)/#\1/' ./spawn_redis_server.sh\n" > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
# Once the heave steps are done, we can copy all files back
COPY . /app
