#!/bin/sh
#
# DON'T EDIT THIS!
#
# CodeCrafters uses this file to test your code. Don't make any changes here!
#
# DON'T EDIT THIS!
set -e
export PATH=/cmake-portable/bin:$PATH
cmake --build ./build
# ls -la build
# cmake . >/dev/null
# make >/dev/null
exec ./build/server "$@"
