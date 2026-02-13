#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

# The option is needed for Java 17+ to allow native memory access operations.
SBT_OPTS="--enable-native-access=ALL-UNNAMED" sbt assembly
