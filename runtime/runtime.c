#include "runtime.h"

#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "gc.h"

void* simpl_malloc(size_t size) {
  return GC_MALLOC(size);
}

inline struct simpl_string* simpl_string_new(size_t byte_count, char* data) {
  struct simpl_string* s = simpl_malloc(sizeof(struct simpl_string));
  *s = (struct simpl_string) {
    .byte_count = byte_count,
    .data = data
  };
  return s;
}

struct simpl_string* simpl_string_slice(struct simpl_string* s, size_t begin, size_t end) {
  assert(s != NULL);
  assert(begin < s->byte_count);
  return simpl_string_new(s->byte_count - (end - begin), s->data + begin);
}

struct simpl_string* simpl_from_cstring(char* cstring) {
  size_t len = strlen(cstring);
  char* data = simpl_malloc(len);
  memcpy(data, cstring, len);
  return simpl_string_new(len, data);
}

char* simpl_string_cstring(const struct simpl_string* s) {
  char* data = malloc(s->byte_count + 1);
  memcpy(data, s->data, s->byte_count);
  data[s->byte_count] = '\0';
  return data;
}

int simpl_string_print(const struct simpl_string* s) {
  char* cstring = simpl_string_cstring(s);
  printf("%s\n", cstring);
  free(cstring);
  return 0;
}
