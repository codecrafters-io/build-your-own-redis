# syntax=docker/dockerfile:1.7-labs
FROM mcr.microsoft.com/dotnet/sdk:6.0-bullseye-slim


RUN mkdir /app/src
RUN (echo 'System.Console.WriteLine("If you are seeing this, there is something wrong with our caching mechanism! Please contact us at hello@codecrafters.io.");' > /app/src/Program.cs) > /dev/null

WORKDIR /app
# .git & README.md are unique per-repository. We ignore them on first copy to prevent cache misses
COPY --exclude=.git --exclude=README.md . /app

# This saves nuget packages to ~/.nuget
RUN dotnet build --configuration Release .

RUN rm /app/src/Program.cs

# This seems to cause a caching issue with the dotnet build command, where contents from the removed /src/Program.cs are used
RUN rm -rf /app/obj
RUN rm -rf /app/bin

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && dotnet build --configuration Release ." > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="codecrafters-redis.csproj,codecrafters-redis.sln"


# Once the heave steps are done, we can copy all files back
COPY . /app
