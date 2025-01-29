# -*- mode: snippet -*-
# name: build.sh
# key: build
# --

#!/usr/bin/env sh
set -e

CFLAGS="-Wall -Wextra -pedantic -std=c11"
CLIBS="$2"

gcc $CFLAGS -o $1 main.c $CLIBS

if [ "\$1" = "run" ]
then
    shift
    ./$1 "$@"
fi
