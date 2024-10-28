#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

# Check if the build directory exists
if [ -d "/tmp/codecrafters-build-redis-swift" ]; then
    echo "Build directory already exists. Proceeding with the build..."

    # This does NOT trigger a rebuild
    # mv /tmp/codecrafters-build-redis-swift /tmp/codecrafters-build-redis-swift-old
    # mv /tmp/codecrafters-build-redis-swift-old /tmp/codecrafters-build-redis-swift

    # This DOES trigger a rebuild! inode changes?
    # mv /tmp/codecrafters-build-redis-swift /tmp/codecrafters-build-redis-swift-old
    # cp -R /tmp/codecrafters-build-redis-swift-old /tmp/codecrafters-build-redis-swift

    mv /tmp/codecrafters-build-redis-swift /tmp/codecrafters-build-redis-swift-old
    cp -p -R /tmp/codecrafters-build-redis-swift-old /tmp/codecrafters-build-redis-swift
else
    echo "Build directory does not exist."
fi

swift build -c release --build-path /tmp/codecrafters-build-redis-swift
