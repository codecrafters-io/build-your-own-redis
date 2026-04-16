#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

LIBS_DIR="/tmp/codecrafters-libs-redis-kotlin"
BUILD_DIR="/tmp/codecrafters-build-redis-kotlin"
KOTLIN_MAIN="$BUILD_DIR/classes/kotlin/main"
KOTLIN_JVM_OPTS="-J--enable-native-access=ALL-UNNAMED -J--sun-misc-unsafe-memory-access=allow"

if [ -d "$LIBS_DIR" ] && [ "$(ls -A "$LIBS_DIR" 2>/dev/null)" ] && command -v kotlinc >/dev/null 2>&1; then
  # Fast path: compile with kotlinc using pre-cached libraries
  mkdir -p "$KOTLIN_MAIN"
  find app/src/main/kotlin -type f -name '*.kt' -exec kotlinc $KOTLIN_JVM_OPTS -cp "$(echo "$LIBS_DIR"/*.jar | tr ' ' ':')" -d "$KOTLIN_MAIN" {} +
else
  # Full build: use Gradle to build and install distribution
  gradle installDist
  mkdir -p "$LIBS_DIR"
  for f in "$BUILD_DIR/install/app/lib/"*.jar; do
    [ "$(basename "$f")" != "app.jar" ] && cp "$f" "$LIBS_DIR/"
  done
fi
