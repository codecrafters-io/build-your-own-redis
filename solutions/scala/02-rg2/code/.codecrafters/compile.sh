#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

scala-cli package src/main/scala/ \
  -q --power --assembly --force --server=false --scala-version=3.8.3 \
  -o /tmp/codecrafters-build-redis-scala