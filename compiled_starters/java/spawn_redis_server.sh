#!/bin/sh
set -e
mvn -B --quiet package -Ddir=/tmp/codecrafters-redis-target
exec java -jar /tmp/codecrafters-bittorrent-target/redis_bittorrent.jar "$@"