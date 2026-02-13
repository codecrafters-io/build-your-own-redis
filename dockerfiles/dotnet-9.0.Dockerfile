# syntax=docker/dockerfile:1.7-labs
FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine

WORKDIR /app

# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# This saves nuget packages to ~/.nuget
RUN dotnet build --configuration Release . \
    && rm -rf /app/obj /app/bin \
    && echo "cd \${CODECRAFTERS_REPOSITORY_DIR} && dotnet build --configuration Release ." > /codecrafters-precompile.sh \
    && chmod +x /codecrafters-precompile.sh

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="CodeCrafters.Redis.csproj,CodeCrafters.Redis.sln"
