#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include "runtime.h"

struct simpl_string simpl_string_new(size_t byte_count, char* data) {
  return (struct simpl_string) {
    .byte_count = byte_count,
    .data = data
  };
}

struct simpl_string simpl_string_slice(struct simpl_string* s, size_t begin, size_t end) {
  assert(s != NULL);
  assert(begin < s->byte_count);
  return (struct simpl_string) {
    .byte_count = s->byte_count - (end - begin),
    .data = s->data + begin
  };
}
