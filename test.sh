#!/bin/bash
set -e
COMPILER_ARGS="$@"
FILENAME="sample"
stack build --fast
stack exec simplc -- "$FILENAME.spl" $COMPILER_ARGS
clang "$FILENAME.o" -o "$FILENAME"
./$FILENAME
