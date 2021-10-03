#include "response_resolver.h"

#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "safe_memory.h"
#include "buffered_reader.h"
#include "string_buffer.h"
#include "logger.h"

typedef enum response_type {
  RESPONSE_200_OK,
  RESPONSE_NOT_200
} response_type_t;

typedef enum response_encoding_type {
  RESPONSE_ENCODING_CHUNKED,
  RESPONSE_ENCODING_NORMAL
} response_encoding_type_t;

static response_type_t resolve_response_type(response_resolver_persistence_t *response_resolver_persistence);
static void load_response_status_chars_to_buffer(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence);
static response_type_t compare_status_chars_with_regex_and_get_type(string_buffer_persistence_t *string_buffer_persistence);
static void report_response_not_200(response_resolver_persistence_t *response_resolver_persistence);
static void report_response_200_ok(response_resolver_persistence_t *response_resolver_persistence);
static char read_and_consume_char(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence);
static void print_cookie_if_appear(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence);
static void print_cookies_and_update_encoding(response_resolver_persistence_t *response_resolver_persistence);
static void update_response_encoding(response_resolver_persistence_t *response_resolver_persistence);
static void print_response_size(response_resolver_persistence_t *response_resolver_persistence);
static void print_response_size_normal_encoding(response_resolver_persistence_t *response_resolver_persistence);
static void print_response_size_chunked_encoding(response_resolver_persistence_t *response_resolver_persistence);
static void reset_string_buffer(string_buffer_persistence_t *string_buffer_persistence);
static size_t calculate_chunked_response_size(response_resolver_persistence_t *response_resolver_persistence);
static size_t calculate_chunk_size_and_skip(response_resolver_persistence_t *response_resolver_persistence);
static int is_end_of_chunk(string_buffer_persistence_t *string_buffer_persistence,
  size_t number_buffer_size, size_t number_buffer_end);
static size_t string_to_int(char *number_buffer, size_t number_buffer_index);
static void update_string_buffer_if_needed(string_buffer_persistence_t *string_buffer_persistence, int end_string_index);
static int is_encoding_chunked(response_resolver_persistence_t *response_resolver_persistence);

static const size_t BUFFER_SIZE = 64;
static const char *RESPONSE_200_OK_REGEX = "HTTP/*.* 200";
static const char *CRLF = "\r\n";
static const char *SEMICOLON = ";*";
static const char *COMA = ",*";
static const char *CRLFCRLF = "\r\n\r\n";
static const char NEW_LINE = '\n';
static const char *SET_COOKIE = "\r\nset-cookie: ";
static const char *RESPONSE_ENCODING_CHUNKED_HEADER_PATTERN = "transfer-encoding:";
static const char *RESPONSE_ENCODING_CHUNKED_PATTERN = "chunked";
static const char *RESPONSE_SIZE_PREFIX = "Dlugosc zasobu:";

typedef struct response_resolver_persistence {
  buffered_reader_persistence_t *buffered_reader_persistence;
  string_buffer_persistence_t *string_buffer_persistence;

  response_encoding_type_t response_encoding_type;
} response_resolver_persistence_t;

response_resolver_persistence_t *init_response_resolver_persistence(tcp_client_persistence_t *tcp_client_persistence) {
  response_resolver_persistence_t *response_resolver_persistence;
  response_resolver_persistence = malloc_or_exit_on_failure(sizeof(response_resolver_persistence_t));

  response_resolver_persistence->buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);
  response_resolver_persistence->string_buffer_persistence = init_string_buffer_persistence(BUFFER_SIZE);

  response_resolver_persistence->response_encoding_type = RESPONSE_ENCODING_NORMAL;

  return response_resolver_persistence;
}

void report_response(response_resolver_persistence_t *response_resolver_persistence) {
  response_type_t response_type = resolve_response_type(response_resolver_persistence);

  switch (response_type) {
    case RESPONSE_200_OK:
      report_response_200_ok(response_resolver_persistence);
      break;

    case RESPONSE_NOT_200:
      report_response_not_200(response_resolver_persistence);
      break;

    default:
      log_and_exit(1, "response type resolver error");
  }
}

response_type_t resolve_response_type(response_resolver_persistence_t *response_resolver_persistence) {
  buffered_reader_persistence_t *buffered_reader_persistence = response_resolver_persistence->buffered_reader_persistence;
  string_buffer_persistence_t *string_buffer_persistence = response_resolver_persistence->string_buffer_persistence;

  load_response_status_chars_to_buffer(buffered_reader_persistence, string_buffer_persistence);

  return compare_status_chars_with_regex_and_get_type(string_buffer_persistence);
}

void load_response_status_chars_to_buffer(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence) {
  const size_t response_regex_size = strlen(RESPONSE_200_OK_REGEX);

  for (size_t i = 0; i < response_regex_size; i++) {
    read_and_consume_char(buffered_reader_persistence, string_buffer_persistence);
  }
}

response_type_t compare_status_chars_with_regex_and_get_type(string_buffer_persistence_t *string_buffer_persistence) {
  if (does_contain_string(string_buffer_persistence, RESPONSE_200_OK_REGEX)) {
    return RESPONSE_200_OK;
  } else {
    return RESPONSE_NOT_200;
  }
}

void report_response_not_200(response_resolver_persistence_t *response_resolver_persistence) {
  buffered_reader_persistence_t *buffered_reader_persistence = response_resolver_persistence->buffered_reader_persistence;
  string_buffer_persistence_t *string_buffer_persistence = response_resolver_persistence->string_buffer_persistence;
  const size_t response_regex_size = strlen(RESPONSE_200_OK_REGEX);

  print_last_n_chars(string_buffer_persistence, response_regex_size);
  print_all_to_given_string(buffered_reader_persistence, CRLF);
  printf("\n");
}

void report_response_200_ok(response_resolver_persistence_t *response_resolver_persistence) {
  print_cookies_and_update_encoding(response_resolver_persistence);
  print_response_size(response_resolver_persistence);
  printf("\n");
}

void print_cookies_and_update_encoding(response_resolver_persistence_t *response_resolver_persistence) {
  buffered_reader_persistence_t *buffered_reader_persistence = response_resolver_persistence->buffered_reader_persistence;
  string_buffer_persistence_t *string_buffer_persistence = response_resolver_persistence->string_buffer_persistence;

  while(does_contain_string(string_buffer_persistence, CRLFCRLF) == 0) {
    read_and_consume_char(buffered_reader_persistence, string_buffer_persistence);
    print_cookie_if_appear(buffered_reader_persistence, string_buffer_persistence);
    update_response_encoding(response_resolver_persistence);
  }
}

void update_response_encoding(response_resolver_persistence_t *response_resolver_persistence) {
  string_buffer_persistence_t *string_buffer_persistence = response_resolver_persistence->string_buffer_persistence;

  if (does_contain_string_non_case_sensitive(string_buffer_persistence, RESPONSE_ENCODING_CHUNKED_HEADER_PATTERN)
    && is_encoding_chunked(response_resolver_persistence)) {
    response_resolver_persistence->response_encoding_type = RESPONSE_ENCODING_CHUNKED;
  }
}

int is_encoding_chunked(response_resolver_persistence_t *response_resolver_persistence) {
  buffered_reader_persistence_t *buffered_reader_persistence = response_resolver_persistence->buffered_reader_persistence;
  string_buffer_persistence_t *string_buffer_persistence = response_resolver_persistence->string_buffer_persistence;

  while(does_contain_string(string_buffer_persistence, CRLF) == 0) {
    read_and_consume_char(buffered_reader_persistence, string_buffer_persistence);

    if (does_contain_string(string_buffer_persistence, RESPONSE_ENCODING_CHUNKED_PATTERN)) {
      return 1;
    }
  }

  return 0;
}

void print_response_size(response_resolver_persistence_t *response_resolver_persistence) {
  switch (response_resolver_persistence->response_encoding_type) {
    case RESPONSE_ENCODING_CHUNKED:
      print_response_size_chunked_encoding(response_resolver_persistence);
      break;
    case RESPONSE_ENCODING_NORMAL:
      print_response_size_normal_encoding(response_resolver_persistence);
      break;
    default:
      log_and_exit(1, "response encoding type resolver error");
  }
}

void print_response_size_normal_encoding(response_resolver_persistence_t *response_resolver_persistence) {
  buffered_reader_persistence_t *buffered_reader_persistence = response_resolver_persistence->buffered_reader_persistence;
  const size_t response_size = read_until_end_and_give_size(buffered_reader_persistence);

  printf("%s %zu", RESPONSE_SIZE_PREFIX, response_size);
}

void print_response_size_chunked_encoding(response_resolver_persistence_t *response_resolver_persistence) {
  size_t response_size = calculate_chunked_response_size(response_resolver_persistence);

  printf("%s %zu", RESPONSE_SIZE_PREFIX, response_size);
}

size_t calculate_chunked_response_size(response_resolver_persistence_t *response_resolver_persistence) {
  size_t response_size = 0;
  size_t chunk_size = 1;

  while(chunk_size > 0) {
    chunk_size = calculate_chunk_size_and_skip(response_resolver_persistence);
    response_size += chunk_size;
  }

  return response_size;
}

size_t calculate_chunk_size_and_skip(response_resolver_persistence_t *response_resolver_persistence) {
  buffered_reader_persistence_t *buffered_reader_persistence = response_resolver_persistence->buffered_reader_persistence;
  string_buffer_persistence_t *string_buffer_persistence = response_resolver_persistence->string_buffer_persistence;

  const size_t number_buffer_size = 64;
  char number_buffer[number_buffer_size];
  size_t number_buffer_index;
  memset(number_buffer, 0, number_buffer_size);
  reset_string_buffer(string_buffer_persistence);

  for (number_buffer_index = 0; is_end_of_chunk(string_buffer_persistence, number_buffer_size, number_buffer_index); number_buffer_index++) {
    char c = read_and_consume_char(buffered_reader_persistence, string_buffer_persistence);
    number_buffer[number_buffer_index] = c;
  }

  size_t chunk_size = string_to_int(number_buffer, number_buffer_index - 2);
  skip_n_next_chars(buffered_reader_persistence, chunk_size + 2);

  return chunk_size;
}

size_t string_to_int(char *number_buffer, size_t number_buffer_end) {
  number_buffer[number_buffer_end] = '\0';
  const size_t chunk_size = strtol(number_buffer, NULL, 16);

  return chunk_size;
}

int is_end_of_chunk(string_buffer_persistence_t *string_buffer_persistence,
  const size_t number_buffer_size, size_t number_buffer_index) {
  return number_buffer_index < number_buffer_size
      && does_contain_string(string_buffer_persistence, CRLF) == 0;
}

void reset_string_buffer(string_buffer_persistence_t *string_buffer_persistence) {
  consume_char(string_buffer_persistence, 'a');
}

void print_cookie_if_appear(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence) {

  if (does_contain_string_non_case_sensitive(string_buffer_persistence, SET_COOKIE)) {
    const int end_string_index = print_all_to_given_strings(buffered_reader_persistence, CRLF, SEMICOLON, COMA);
    update_string_buffer_if_needed(string_buffer_persistence, end_string_index);

    printf("%c", NEW_LINE);
  }
}

void update_string_buffer_if_needed(string_buffer_persistence_t *string_buffer_persistence, int end_string_index) {
  if (end_string_index == 1) {
    consume_char(string_buffer_persistence, '\r');
    consume_char(string_buffer_persistence, '\n');
  }
}

char read_and_consume_char(buffered_reader_persistence_t *buffered_reader_persistence,
  string_buffer_persistence_t *string_buffer_persistence) {
  const char c = get_one_char(buffered_reader_persistence);
  consume_char(string_buffer_persistence, c);

  return c;
}

void delete_response_resolver_persistence(response_resolver_persistence_t *response_resolver_persistence) {
  delete_buffered_reader_persistence(response_resolver_persistence->buffered_reader_persistence);
  delete_string_buffer_persistence(response_resolver_persistence->string_buffer_persistence);

  free(response_resolver_persistence);
}
