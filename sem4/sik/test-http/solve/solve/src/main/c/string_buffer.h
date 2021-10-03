#ifndef MIMUW_S4_SIK_TESTHTTP_STRING_BUFFER_H
#define MIMUW_S4_SIK_TESTHTTP_STRING_BUFFER_H

#include <stdlib.h>

typedef struct string_buffer_persistence string_buffer_persistence_t;

string_buffer_persistence_t *init_string_buffer_persistence(size_t buffer_size);
void consume_char(string_buffer_persistence_t *string_buffer_persistence, char c);
size_t does_contain_string(string_buffer_persistence_t *string_buffer_persistence, const char *pattern);
size_t does_contain_string_non_case_sensitive(string_buffer_persistence_t *string_buffer_persistence, const char *pattern);
void print_last_n_chars(string_buffer_persistence_t *string_buffer_persistence, size_t n);
void print_first_char(string_buffer_persistence_t *string_buffer_persistence);
void delete_string_buffer_persistence(string_buffer_persistence_t *string_buffer_persistence);

#endif //MIMUW_S4_SIK_TESTHTTP_STRING_BUFFER_H
