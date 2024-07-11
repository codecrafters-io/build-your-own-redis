#!/bin/sh
#
# DON'T EDIT THIS!
#
# CodeCrafters uses this file to test your code. Don't make any changes here!
#
# DON'T EDIT THIS!
set -e
lein deps
lein uberjar
exec java -jar /tmp/codecrafters-redis-target/uberjar/redis-0.1.0-SNAPSHOT-standalone.jar "$@"