FROM mcr.microsoft.com/dotnet/sdk:6.0-bullseye-slim

COPY codecrafters-redis.csproj /app/codecrafters-redis.csproj
COPY codecrafters-redis.sln /app/codecrafters-redis.sln

RUN mkdir /app/src
RUN echo 'System.Console.WriteLine("Hello World 2!");' > /app/src/Program.cs

WORKDIR /app

RUN dotnet run --project . --configuration Release "$@" # This saves nuget packages to ~/.nuget

RUN mkdir /app-cached
RUN mv /app/obj /app-cached/obj
RUN mv /app/bin /app-cached/bin

RUN echo "cd \${CODECRAFTERS_SUBMISSION_DIR} && dotnet build --configuration Release ." > /codecrafters-precompile.sh
RUN chmod +x /codecrafters-precompile.sh
