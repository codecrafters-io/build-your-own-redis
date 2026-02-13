#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

stackInstallRoot=$(cd $(dirname "$0") && stack path --local-install-root) # Fetch the path from within the project directory
exec "$stackInstallRoot/bin/codecrafters-redis-exe" "$@"