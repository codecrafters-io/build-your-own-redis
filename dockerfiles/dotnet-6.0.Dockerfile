FROM mcr.microsoft.com/dotnet/sdk:6.0-bullseye-slim

COPY codecrafters-redis.csproj /app/codecrafters-redis.csproj
COPY codecrafters-redis.sln /app/codecrafters-redis.sln

RUN mkdir /app/src
RUN echo 'System.Console.WriteLine("If you are seeing this, there is something wrong with our caching mechanism! Please contact us at hello@codecrafters.io.");' > /app/src/Program.cs

WORKDIR /app

RUN dotnet run --project . --configuration Release "$@" # This saves nuget packages to ~/.nuget
RUN rm /app/src/Program.cs

# This seems to cause a caching issue with the dotnet build command, where contents from the removed /src/Program.cs are used
RUN rm -rf /app/obj
RUN rm -rf /app/bin

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && dotnet build --configuration Release ." > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh

ENV CODECRAFTERS_DEPENDENCY_FILE_PATHS="codecrafters-redis.csproj,codecrafters-redis.sln"

