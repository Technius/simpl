#!/bin/bash
set -e
COMPILER_ARGS="$@"
stack build --fast
make -C runtime libgc.a

TEST_DIR=test-suite
TEST_BIN_DIR="$TEST_DIR/bin"
mkdir -p $TEST_BIN_DIR

for src_file in $(find "$TEST_DIR" -type f -name '*.spl'); do
    out_name=$(basename "${src_file%.spl}.o")
    out_name="$TEST_BIN_DIR/$out_name"
    echo "Building ${out_name%.o}..."
    stack exec simplc -- "$src_file" -o "$out_name" $COMPILER_ARGS
    clang -g -pthread runtime/libgc.a "$out_name" -o "${out_name%.o}" -lm
done
