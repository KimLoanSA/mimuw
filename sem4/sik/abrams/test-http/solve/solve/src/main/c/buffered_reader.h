#ifndef MIMUW_S4_SIK_TESTHTTP_BUFFERED_READER_H
#define MIMUW_S4_SIK_TESTHTTP_BUFFERED_READER_H

#include <stdlib.h>

#include "tcp_client.h"

typedef struct buffered_reader_persistence buffered_reader_persistence_t;

buffered_reader_persistence_t *init_buffered_reader_persistence(tcp_client_persistence_t *tcp_client_persistence);
char get_one_char(buffered_reader_persistence_t *buffered_reader_persistence);
void skip_n_next_chars(buffered_reader_persistence_t *buffered_reader_persistence, size_t n);
void print_all_to_given_string(buffered_reader_persistence_t *buffered_reader_persistence, const char *end_string);
int print_all_to_given_strings(buffered_reader_persistence_t *buffered_reader_persistence,
  const char *end_string_1, const char *end_string_2, const char *end_string_3);
size_t read_until_end_and_give_size(buffered_reader_persistence_t *buffered_reader_persistence);
int is_empty(buffered_reader_persistence_t *buffered_reader_persistence);
void delete_buffered_reader_persistence(buffered_reader_persistence_t *buffered_reader_persistence);

#endif //MIMUW_S4_SIK_TESTHTTP_BUFFERED_READER_H
