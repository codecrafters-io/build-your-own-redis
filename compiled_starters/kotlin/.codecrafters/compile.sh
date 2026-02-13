#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

gradle distTar
cd /tmp/codecrafters-build-redis-kotlin/distributions
rm -rf app
tar -xvf app.tar
