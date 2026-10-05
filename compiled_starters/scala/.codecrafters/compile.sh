#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

scala-cli package src/main/scala/ \
  -q --power --assembly --force --server=false --scala-version=3.9.0 \
  --main-class codecrafters_redis.Server \
  -o /tmp/codecrafters-build-redis-scala