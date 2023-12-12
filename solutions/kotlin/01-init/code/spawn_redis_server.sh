#!/bin/sh
set -e
mvn -B --quiet package -Ddir=/tmp/codecrafters-redis-target
exec java -jar /tmp/codecrafters-redis-target/kotlin_redis.jar "$@"