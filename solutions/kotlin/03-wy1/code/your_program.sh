#!/bin/sh
#
# Use this script to run your program LOCALLY.
#
# Note: Changing this script WILL NOT affect how CodeCrafters runs your program.
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit early if any commands fail

# Copied from .codecrafters/compile.sh
#
# - Edit this to change how your program compiles locally
# - Edit .codecrafters/compile.sh to change how your program compiles remotely
(
  cd "$(dirname "$0")" # Ensure compile steps are run within the repository directory
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
)

# Copied from .codecrafters/run.sh
#
# - Edit this to change how your program runs locally
# - Edit .codecrafters/run.sh to change how your program runs remotely
LIBS_DIR="/tmp/codecrafters-libs-redis-kotlin"
BUILD_DIR="/tmp/codecrafters-build-redis-kotlin"
KOTLIN_MAIN="$BUILD_DIR/classes/kotlin/main"
exec java -cp "$KOTLIN_MAIN:$LIBS_DIR/*" AppKt "$@"
