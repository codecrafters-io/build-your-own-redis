#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

# TODO: Fill this in
#!/bin/sh
set -e
mvn -B --quiet package -Ddir=/tmp/codecrafters-redis-target
exec java -jar /tmp/codecrafters-redis-target/java_redis.jar "$@"