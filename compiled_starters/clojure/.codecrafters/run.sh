#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

exec java -jar /tmp/codecrafters-redis-target/uberjar/redis-0.1.0-SNAPSHOT-standalone.jar "$@"