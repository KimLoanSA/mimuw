#include "string_buffer.h"

#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>

#include "safe_memory.h"


static void increment_index_and_module_if_needed(string_buffer_persistence_t *string_buffer_persistence);
static size_t increment_and_module_if_needed(size_t number, size_t modulo);
static size_t subtract_and_module_if_needed(size_t number1, size_t number2, size_t modulo);
static size_t compare_buffer_with_string(string_buffer_persistence_t *string_buffer_persistence, const char *string, size_t string_size);
static size_t compare_buffer_with_string_non_case_sensitive(string_buffer_persistence_t *string_buffer_persistence,
  const char *string, size_t string_size);
static int compare_chars_with_wildcard_for_second(char c1, char c2, char wildcard);
static int compare_chars_with_wildcard_for_second_non_case_sensitive(char c1, char c2, char wildcard);
static const char WILDCARD_CHAR = '*';

typedef struct string_buffer_persistence {
  char *buffer;

  size_t buffer_size;
  size_t buffer_index;
} string_buffer_persistence_t;

string_buffer_persistence_t* init_string_buffer_persistence(size_t buffer_size) {
  string_buffer_persistence_t *string_buffer_persistence;

  string_buffer_persistence = malloc_or_exit_on_failure(sizeof(string_buffer_persistence_t));

  string_buffer_persistence->buffer = malloc_or_exit_on_failure(sizeof(char) * buffer_size);
  string_buffer_persistence->buffer_size = buffer_size;
  string_buffer_persistence->buffer_index = 0;

  return string_buffer_persistence;
}

void consume_char(string_buffer_persistence_t *string_buffer_persistence, char c) {
  string_buffer_persistence->buffer[string_buffer_persistence->buffer_index] = c;

  increment_index_and_module_if_needed(string_buffer_persistence);
}

void increment_index_and_module_if_needed(string_buffer_persistence_t *string_buffer_persistence) {
  size_t index = string_buffer_persistence->buffer_index;
  size_t modulo = string_buffer_persistence->buffer_size;

  string_buffer_persistence->buffer_index = increment_and_module_if_needed(index, modulo);
}

size_t does_contain_string(string_buffer_persistence_t *string_buffer_persistence, const char *pattern) {
  const size_t pattern_size = strlen(pattern);

  return compare_buffer_with_string(string_buffer_persistence, pattern, pattern_size);
}

size_t does_contain_string_non_case_sensitive(string_buffer_persistence_t *string_buffer_persistence, const char *pattern) {
  const size_t pattern_size = strlen(pattern);

  return compare_buffer_with_string_non_case_sensitive(string_buffer_persistence, pattern, pattern_size);
}

void print_last_n_chars(string_buffer_persistence_t *string_buffer_persistence, const size_t n) {
  const size_t buffer_size = string_buffer_persistence->buffer_size;
  const char *buffer = string_buffer_persistence->buffer;
  size_t buffer_index = subtract_and_module_if_needed(string_buffer_persistence->buffer_index, n, buffer_size);

  for (size_t i = 0; i < n; i++) {
    printf("%c", buffer[buffer_index]);
    buffer_index = increment_and_module_if_needed(buffer_index, buffer_size);
  }
}

void print_first_char(string_buffer_persistence_t *string_buffer_persistence) {
  const size_t buffer_index = string_buffer_persistence->buffer_index;
  const char *buffer = string_buffer_persistence->buffer;

  printf("%c", buffer[buffer_index]);
}

size_t compare_buffer_with_string(string_buffer_persistence_t *string_buffer_persistence, const char *string, size_t string_size) {
  const size_t buffer_size = string_buffer_persistence->buffer_size;
  const char *buffer = string_buffer_persistence->buffer;
  size_t buffer_index = subtract_and_module_if_needed(string_buffer_persistence->buffer_index, string_size, buffer_size);

  if (buffer_size < string_size) {
    return 0;
  }

  for (size_t string_index = 0; string_index < string_size; string_index++) {
    if (compare_chars_with_wildcard_for_second(buffer[buffer_index], string[string_index], WILDCARD_CHAR) == 0) {
      return 0;
    }

    buffer_index = increment_and_module_if_needed(buffer_index, buffer_size);
  }

  return string_size;
}

size_t compare_buffer_with_string_non_case_sensitive(string_buffer_persistence_t *string_buffer_persistence,
  const char *string, size_t string_size) {
  const size_t buffer_size = string_buffer_persistence->buffer_size;
  const char *buffer = string_buffer_persistence->buffer;
  size_t buffer_index = subtract_and_module_if_needed(string_buffer_persistence->buffer_index, string_size, buffer_size);

  if (buffer_size < string_size) {
    return 0;
  }

  for (size_t string_index = 0; string_index < string_size; string_index++) {
    if (compare_chars_with_wildcard_for_second_non_case_sensitive(buffer[buffer_index], string[string_index], WILDCARD_CHAR) == 0) {
      return 0;
    }

    buffer_index = increment_and_module_if_needed(buffer_index, buffer_size);
  }

  return string_size;
}

size_t increment_and_module_if_needed(size_t number, size_t modulo) {
  return (number + 1) % modulo;
}

size_t subtract_and_module_if_needed(const size_t number1, const size_t number2, const size_t modulo) {
  ssize_t result = number1 - number2;

  if (result < 0) {
    result = (ssize_t) modulo + result;
  }

  return result;
}

int compare_chars_with_wildcard_for_second(const char c1, const char c2, const char wildcard) {
  return c2 == wildcard || c1 == c2;
}

int compare_chars_with_wildcard_for_second_non_case_sensitive(const char c1, const char c2, const char wildcard) {
  return c2 == wildcard || tolower(c1) == tolower(c2);
}

void delete_string_buffer_persistence(string_buffer_persistence_t *string_buffer_persistence) {
  free(string_buffer_persistence->buffer);
  free(string_buffer_persistence);
}
