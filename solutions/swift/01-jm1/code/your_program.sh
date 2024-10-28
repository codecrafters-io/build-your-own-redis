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
  if [ -d "/tmp/codecrafters-build-redis-swift" ]; then
      echo "Build directory already exists. Proceeding with the build..."
      # This does NOT trigger a rebuild
      mv /tmp/codecrafters-build-redis-swift /tmp/codecrafters-build-redis-swift-old
      mv /tmp/codecrafters-build-redis-swift-old /tmp/codecrafters-build-redis-swift
      # This DOES trigger a rebuild! inode changes?
      # cp -R /tmp/codecrafters-build-redis-swift-old /tmp/codecrafters-build-redis-swift
  else
      echo "Build directory does not exist."
  fi
  swift build -c release --build-path /tmp/codecrafters-build-redis-swift
)

# Copied from .codecrafters/run.sh
#
# - Edit this to change how your program runs locally
# - Edit .codecrafters/run.sh to change how your program runs remotely
exec swift run -c release --skip-build --build-path /tmp/codecrafters-build-redis-swift build-your-own-redis "$@"
