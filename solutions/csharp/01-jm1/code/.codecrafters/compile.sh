#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

dotnet build --configuration Release --output /tmp/codecrafters-build-redis-csharp codecrafters-redis.csproj
