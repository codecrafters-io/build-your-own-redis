# syntax=docker/dockerfile:1.7-labs
FROM mcr.microsoft.com/dotnet/sdk:10.0-alpine

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="codecrafters-redis.csproj,codecrafters-redis.sln"


WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

RUN .codecrafters/compile.sh

# This seems to cause a caching issue with the dotnet build command, where old contents are used
# TODO: See if this needs to be brought back?
# RUN rm -rf /app/obj
# RUN rm -rf /app/bin