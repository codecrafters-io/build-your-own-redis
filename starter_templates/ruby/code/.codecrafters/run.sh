#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

export BUNDLE_GEMFILE="$(dirname $0)/Gemfile"
exec bundle exec ruby "$(dirname $0)/app/main.rb" "$@"
