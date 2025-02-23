# -*- mode: snippet -*-
# name: build.sh
# key: build
# --

#!/usr/bin/env sh
set -xe

CFLAGS="-Wall -Wextra -pedantic -ggdb -std=c11"
CLIBS="$2"

gcc $CFLAGS -o $1 main.c $CLIBS
