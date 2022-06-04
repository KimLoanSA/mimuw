#include "address_resolver.h"

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "safe_memory.h"

static long string_to_int_port(const char *port);
static int is_port_valid(size_t string_size, size_t colon_index);
static size_t find_char(const char *string, char c, size_t string_size, size_t char_number);
static const char *get_resource_normal(const char *address, size_t start_index, size_t address_size);
static const char *get_resource_not_normal(const char *address, size_t start_index_1, size_t start_index_2, size_t address_size);
static size_t get_min_ending_index(const char *address, size_t address_size);
static size_t min(size_t value_1, size_t value_2);

const char *get_port(const char *address) {
  const size_t address_size = strlen(address);
  const size_t colon_index = find_char(address, ':', address_size, 1);
  const size_t port_size = address_size - colon_index + 2;

  if (is_port_valid(address_size, colon_index) == 0) {
    return "";
  }

  char *port = malloc_or_exit_on_failure(sizeof(char) * port_size);

  strncpy(port, address + colon_index + 1, port_size);
  port[port_size - 1] = '\0';

  return port;
}

long get_port_long(const char *address) {
  const size_t address_size = strlen(address);
  const size_t colon_index = find_char(address, ':', address_size, 1);

  if (is_port_valid(address_size, colon_index) == 0) {
    return -1;
  }

  return string_to_int_port(address + colon_index + 1);
}

int is_port_valid(const size_t string_size, const size_t colon_index) {
  return (string_size - colon_index) <= 6
    && (string_size - colon_index) > 1;
}

size_t find_colon(const char *string, const size_t string_size) {
  const char colon = ':';

  for (size_t i = 0; i < string_size; i++) {
    if (string[i] == colon) {
      return i;
    }
  }

  return string_size;
}

long string_to_int_port(const char *port) {
  long port_value = strtol(port, NULL, 10);

  return port_value;
}

const char *get_host(const char *address) {
  const size_t address_size = strlen(address);
  const size_t slash_2_index = find_char(address, '/', address_size, 2) + 1;
  const size_t min_ending_index = get_min_ending_index(address, address_size);

  const size_t host_size = min_ending_index - slash_2_index + 2;

  char *host = malloc_or_exit_on_failure(sizeof(char) * host_size);

  strncpy(host, address + slash_2_index, host_size);
  host[host_size - 1] = '\0';

  return host;
}

size_t get_min_ending_index(const char *address, const size_t address_size) {
  const size_t slash_3_index = find_char(address, '/', address_size, 3) - 1;
  const size_t hash_index = find_char(address, '#', address_size, 1) - 1;
  const size_t question_mark_index = find_char(address, '?', address_size, 1) - 1;

  return min(slash_3_index, min(hash_index, question_mark_index));
}

size_t min(size_t value_1, size_t value_2) {
  return value_1 < value_2 ? value_1 : value_2;
}

const char *get_host_without_port(const char *address) {
  const size_t address_size = strlen(address);
  const size_t colon_index = find_colon(address, address_size);
  const size_t host_size = colon_index + 1;

  char *host = malloc_or_exit_on_failure(sizeof(char) * host_size);

  strncpy(host, address, host_size);
  host[host_size - 1] = '\0';

  return host;
}

const char *get_resource(const char *address) {
  const size_t address_size = strlen(address);
  const size_t slash_3_index = find_char(address, '/', address_size, 3) + 1;
  const size_t hash_index = find_char(address, '#', address_size, 1);
  const size_t question_mark_index = find_char(address, '?', address_size, 1);

  if (slash_3_index < hash_index && slash_3_index < question_mark_index) {
    return get_resource_normal(address, slash_3_index, find_char(address, '#', address_size, 1) - 1);
  } else {
    return get_resource_not_normal(address, hash_index, question_mark_index, find_char(address, '#', address_size, 1) - 1);
  }
}

const char *get_resource_not_normal(const char *address, const size_t start_index_1, const size_t start_index_2, const size_t address_size) {
  if (start_index_1 < start_index_2) {
    return get_resource_normal(address, start_index_1, address_size);
  } else {
    return get_resource_normal(address, start_index_2, address_size);
  }
}

const char *get_resource_normal(const char *address, const size_t start_index, const size_t address_size) {
  const size_t resource_size = address_size - start_index + 3;
  char *resource = malloc_or_exit_on_failure(sizeof(char) * resource_size);

  strncpy(resource, address + start_index - 1, resource_size);
  resource[resource_size - 1] = '\0';
  resource[0] = '/';

  return resource;
}

size_t find_char(const char *string, char c, size_t string_size, size_t char_number) {
  size_t slash_counter = 0;

  for (size_t i = 0; i < string_size; i++) {
    if (string[i] == c) {
      slash_counter++;

      if (slash_counter == char_number) {
        return i;
      }
    }
  }

  return string_size;
}