#include <stdint.h>
#include <assert.h>
#include <stdlib.h>

#ifndef RUNTIME_H
#define RUNTIME_H

/**
 * An immutable UTF-8 string, as used in SimPL. All data in the string should be
 * initialized. This string is NOT null terminated.
 */
struct simpl_string {
  size_t byte_count;
  char* data;
};

struct simpl_string simpl_string_new(size_t byte_count, char* data);

/**
 * Returns an immutable slice of the suffix of the given string.
 */
struct simpl_string simpl_string_slice(struct simpl_string* s, size_t begin, size_t end);

#endif
