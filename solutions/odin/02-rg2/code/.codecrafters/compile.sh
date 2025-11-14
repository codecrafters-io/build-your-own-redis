#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

mkdir -p /tmp/codecrafters-build-redis-odin
odin build src -out:/tmp/codecrafters-build-redis-odin/main
