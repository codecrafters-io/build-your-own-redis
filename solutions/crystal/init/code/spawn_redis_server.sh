#!/bin/sh
#
# DON'T EDIT THIS!
#
# CodeCrafters uses this file to test your code. Don't make any changes here!
#
# DON'T EDIT THIS!
set -e
tmpfile=$(mktemp)
crystal build -o "$tmpfile" app/main.cr
exec "$tmpfile"
