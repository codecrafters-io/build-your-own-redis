#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

LIBS_DIR="/tmp/codecrafters-libs-redis-kotlin"
BUILD_DIR="/tmp/codecrafters-build-redis-kotlin"
KOTLIN_MAIN="$BUILD_DIR/classes/kotlin/main"

exec java -cp "$KOTLIN_MAIN:$LIBS_DIR/*" AppKt "$@"
