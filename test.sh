#!/bin/bash
set -e
export FILENAME="sample"
stack build --fast
stack exec simplc -- "$FILENAME.spl"
clang "$FILENAME.o" -o "$FILENAME"
./$FILENAME
