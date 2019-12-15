#include <stdlib.h>

#ifndef RUNTIME_H
#define RUNTIME_H

/**
 * Allocates num bytes according to the allocation strategy. This should be
 * called whenever heap memory needs to be allocated in the runtime.
 */
inline void* simpl_malloc(size_t size);

/**
 * An immutable UTF-8 string, as used in SimPL. All data in the string should be
 * initialized. This string is NOT null terminated.
 */
struct simpl_string {
  size_t byte_count;
  char* data;
};

struct simpl_string* simpl_string_new(size_t byte_count, char* data);

/**
 * Returns an immutable slice of the suffix of the given string.
 */
struct simpl_string* simpl_string_slice(struct simpl_string* s, size_t begin, size_t end);

/**
 * Constructs a simpl_string from a null terminated C string. The data is copied
 * from the C string.
 */
struct simpl_string* simpl_from_cstring(char* cstring);

/**
 * Constructs a C string from a simpl_string.
 */
char* simpl_string_cstring(const struct simpl_string* s);

int simpl_string_print(const struct simpl_string* s);

/**
 * Describes static information about a type. This struct should not be visible
 * from SimPL programs.
 */
struct simpl_type_tag {
    /**
     * The size (e.g. when compiled) of the type, in bytes.
     */
    unsigned int size;
};

/**
 * Returns the size recorded in the type tag.
 */
int simpl_tag_size(const struct simpl_type_tag* const);

/**
 * A value tagged with its type tag. Used for polymorphic functions and
 * variables.
 */
struct simpl_tagged_value {
    const struct simpl_type_tag* const type_tag;
    void* data;
};

/**
 * Returns the type tag of a tagged value.
 */
const struct simpl_type_tag* const simpl_tagged_tag(struct simpl_tagged_value*);

/**
 * Returns a pointer to the boxed value of a tagged value.
 */
void* simpl_tagged_unbox(struct simpl_tagged_value*);

/**
 * Boxes the given value
 */
struct simpl_tagged_value* simpl_tagged_box(struct simpl_type_tag* tag, void* data);

#endif
