#!/bin/bash
set -e
COMPILER_ARGS="$@"
FILENAME="sample"
stack build --fast
stack exec simplc -- "$FILENAME.spl" $COMPILER_ARGS
make -C runtime libgc.a
clang -pthread runtime/libgc.a "$FILENAME.o" -o "$FILENAME"
./$FILENAME
