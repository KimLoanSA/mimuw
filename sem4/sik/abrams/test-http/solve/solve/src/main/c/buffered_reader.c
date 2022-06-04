#include "buffered_reader.h"

#include <string.h>
#include <stdio.h>

#include "tcp_client.h"
#include "safe_memory.h"
#include "string_buffer.h"

static size_t get_index_or_load_buffer(buffered_reader_persistence_t *buffered_reader_persistence);
static size_t is_it_end_of_buffer(buffered_reader_persistence_t *buffered_reader_persistence);
static void load_buffer(buffered_reader_persistence_t *buffered_reader_persistence);
static void print_to_ending_strings(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3);
static int is_end_or_equal_string(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3);
static size_t load_buffer_and_get_index(buffered_reader_persistence_t *buffered_reader_persistence);
static size_t get_index(buffered_reader_persistence_t *buffered_reader_persistence);
static void prepare_buffer(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, size_t end_string_size);
static void print_to_ending_string_with_prepared_buffer(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3);
static int resolve_end_string_index(string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3);

typedef struct buffered_reader_persistence {
  tcp_client_persistence_t *tcp_client_persistence;

  char *buffer;

  size_t buffer_index;
  size_t buffer_size;
} buffered_reader_persistence_t;

buffered_reader_persistence_t *init_buffered_reader_persistence(tcp_client_persistence_t *tcp_client_persistence) {
  const size_t buffer_size = get_buffer_size(tcp_client_persistence);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = malloc_or_exit_on_failure(sizeof(buffered_reader_persistence_t));

  buffered_reader_persistence->tcp_client_persistence = tcp_client_persistence;
  buffered_reader_persistence->buffer_size = buffer_size;
  buffered_reader_persistence->buffer_index = 0;
  buffered_reader_persistence->buffer = malloc_or_exit_on_failure(sizeof(char) * buffer_size);

  load_buffer(buffered_reader_persistence);

  return buffered_reader_persistence;
}

char get_one_char(buffered_reader_persistence_t *buffered_reader_persistence) {
  size_t buffer_index = get_index_or_load_buffer(buffered_reader_persistence);

  return buffered_reader_persistence->buffer[buffer_index];
}

size_t get_index_or_load_buffer(buffered_reader_persistence_t *buffered_reader_persistence) {
  if (is_it_end_of_buffer(buffered_reader_persistence)) {
    return load_buffer_and_get_index(buffered_reader_persistence);
  } else {
    return get_index(buffered_reader_persistence);
  }
}

size_t load_buffer_and_get_index(buffered_reader_persistence_t *buffered_reader_persistence) {
  load_buffer(buffered_reader_persistence);
  size_t buffer_index = buffered_reader_persistence->buffer_index;
  buffered_reader_persistence->buffer_index++;

  return buffer_index;
}

size_t get_index(buffered_reader_persistence_t *buffered_reader_persistence) {
  size_t buffer_index = buffered_reader_persistence->buffer_index;
  buffered_reader_persistence->buffer_index++;

  return buffer_index;
}

size_t is_it_end_of_buffer(buffered_reader_persistence_t *buffered_reader_persistence) {
  return buffered_reader_persistence->buffer_index == buffered_reader_persistence->buffer_size;
}

void load_buffer(buffered_reader_persistence_t *buffered_reader_persistence) {
  buffered_reader_persistence->buffer_index = 0;
  buffered_reader_persistence->buffer_size =
    read_and_save_to_buffer(buffered_reader_persistence->tcp_client_persistence, buffered_reader_persistence->buffer);
}

void skip_n_next_chars(buffered_reader_persistence_t *buffered_reader_persistence, const size_t n) {
  for (size_t i = 0; i < n; i++) {
    get_one_char(buffered_reader_persistence);
  }
}

void print_all_to_given_string(buffered_reader_persistence_t *buffered_reader_persistence, const char *end_string) {
  const size_t end_string_size = strlen(end_string);

  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(end_string_size);

  print_to_ending_strings(buffered_reader_persistence,
    string_buffer_persistence, end_string, end_string, end_string);

  delete_string_buffer_persistence(string_buffer_persistence);
}

int print_all_to_given_strings(buffered_reader_persistence_t *buffered_reader_persistence,
  const char *end_string_1, const char *end_string_2, const char *end_string_3) {

  const size_t end_string_size = strlen(end_string_1);

  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(end_string_size);

  print_to_ending_strings(buffered_reader_persistence,
    string_buffer_persistence, end_string_1, end_string_2, end_string_3);

  int end_string_index = resolve_end_string_index(string_buffer_persistence,
    end_string_1, end_string_2, end_string_3);

  delete_string_buffer_persistence(string_buffer_persistence);

  return end_string_index;
}

void print_to_ending_strings(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3) {

  const size_t end_string_size = strlen(end_string_1);

  prepare_buffer(buffered_reader_persistence, string_buffer_persistence, end_string_size);
  print_to_ending_string_with_prepared_buffer(buffered_reader_persistence,
    string_buffer_persistence, end_string_1, end_string_2, end_string_3);
}

void print_to_ending_string_with_prepared_buffer(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1, const char *end_string_2,
  const char *end_string_3) {

  while (is_end_or_equal_string(buffered_reader_persistence, string_buffer_persistence,
    end_string_1, end_string_2, end_string_3) == 0) {

    print_first_char(string_buffer_persistence);

    const char c = get_one_char(buffered_reader_persistence);
    consume_char(string_buffer_persistence, c);
  }
}

void prepare_buffer(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const size_t end_string_size) {

  for (size_t i = 0; i < end_string_size; i++) {
    const char c = get_one_char(buffered_reader_persistence);
    consume_char(string_buffer_persistence, c);
  }
}

int resolve_end_string_index(string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3) {

  if (does_contain_string(string_buffer_persistence, end_string_1)) {
    return 1;
  }

  if (does_contain_string(string_buffer_persistence, end_string_2)) {
    return 2;
  }

  if (does_contain_string(string_buffer_persistence, end_string_3)) {
    return 3;
  }

  return 0;
}

int is_end_or_equal_string(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence, const char *end_string_1,
  const char *end_string_2, const char *end_string_3) {

  return is_empty(buffered_reader_persistence)
    || does_contain_string(string_buffer_persistence, end_string_1)
    || does_contain_string(string_buffer_persistence, end_string_2)
    || does_contain_string(string_buffer_persistence, end_string_3);
}

size_t read_until_end_and_give_size(buffered_reader_persistence_t *buffered_reader_persistence) {
  const size_t buffer_index = buffered_reader_persistence->buffer_index;
  const size_t buffer_size = buffered_reader_persistence->buffer_size;
  tcp_client_persistence_t *tcp_client_persistence = buffered_reader_persistence->tcp_client_persistence;

  size_t read_bytes_sum = buffer_size - buffer_index;

  while(does_connection_ended(tcp_client_persistence) == 0) {
    read_bytes_sum += read_and_save_to_buffer(tcp_client_persistence, buffered_reader_persistence->buffer);
  }

  return read_bytes_sum;
}

int is_empty(buffered_reader_persistence_t *buffered_reader_persistence) {
  const size_t buffer_index = buffered_reader_persistence->buffer_index;
  const size_t buffer_size = buffered_reader_persistence->buffer_size;

  return buffer_index == buffer_size
    && does_connection_ended(buffered_reader_persistence->tcp_client_persistence);
}

void delete_buffered_reader_persistence(buffered_reader_persistence_t *buffered_reader_persistence) {
  free(buffered_reader_persistence->buffer);

  free(buffered_reader_persistence);
}
