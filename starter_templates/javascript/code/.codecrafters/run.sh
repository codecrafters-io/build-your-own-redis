#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

NODE_PATH="$(dirname "$0")/node_modules" \
exec node "$(dirname "$0")/app/main.js" "$@"