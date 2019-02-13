#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include "runtime.h"

#include <stdio.h>

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

struct simpl_string simpl_from_cstring(char* cstring) {
  size_t len = strlen(cstring);
  char* data = malloc(len);
  memcpy(data, cstring, len);
  return (struct simpl_string) {
    .byte_count = len,
    .data = data
  };
}

char* simpl_string_cstring(struct simpl_string s) {
  char* data = malloc(s.byte_count + 1);
  memcpy(data, s.data, s.byte_count);
  data[s.byte_count] = '\0';
  return data;
}

int simpl_string_print(struct simpl_string* s) {
  char* cstring = simpl_string_cstring(*s);
  printf("%s\n", cstring);
  free(cstring);
  return 0;
}
