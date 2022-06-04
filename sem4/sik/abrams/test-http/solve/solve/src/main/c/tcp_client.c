#include "tcp_client.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#include "safe_memory.h"
#include "logger.h"

static void log_and_exit_if_error(int exit_code);
static int open_connection(const char *host, const char *port);
static void open_file_or_exit(tcp_client_persistence_t *tcp_client_persistence);
static void send_first_cookie(FILE *socket_file, const char *cookie);
static void send_next_cookie(FILE *socket_file, const char *cookie);
static void end_request_after_headers(FILE *socket_file);
static void end_request_after_cookies(FILE *socket_file);

typedef struct tcp_client_persistence {
  int socket_value;
  FILE *socket_file;
  int connection_end;

  int cookie_header_send;

  size_t buffer_size;
  size_t max_buffer_size;
} tcp_client_persistence_t;

tcp_client_persistence_t *init_tcp_client_persistence(const char *host, const char*port, const size_t buffer_size) {
  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = malloc_or_exit_on_failure(sizeof(tcp_client_persistence_t));

  tcp_client_persistence->connection_end = 0;
  tcp_client_persistence->cookie_header_send = 0;
  tcp_client_persistence->socket_value = open_connection(host, port);
  tcp_client_persistence->buffer_size = buffer_size;
  tcp_client_persistence->max_buffer_size = buffer_size;
  open_file_or_exit(tcp_client_persistence);

  return tcp_client_persistence;
}

static void open_file_or_exit(tcp_client_persistence_t *tcp_client_persistence) {
  tcp_client_persistence->socket_file = fdopen(tcp_client_persistence->socket_value, "r+");

  if (tcp_client_persistence->socket_file == NULL) {
    log_and_exit(1, "tcp file open ");
  }
}

int open_connection(const char *host, const char *port) {
  int socket_value;
  struct addrinfo addr_hints;
  struct addrinfo *addr_result;

  memset(&addr_hints, 0, sizeof(struct addrinfo));
  addr_hints.ai_family = AF_INET;
  addr_hints.ai_socktype = SOCK_STREAM;
  addr_hints.ai_protocol = IPPROTO_TCP;

  int error_code = getaddrinfo(host, port, &addr_hints, &addr_result);
  if (error_code != 0) {
    log_and_exit(error_code, "getaddrinfo");
  }

  socket_value = socket(addr_result->ai_family, addr_result->ai_socktype, addr_result->ai_protocol);
  if (socket_value < 0) {
    log_and_exit(socket_value, "socket");
  }

  if (connect(socket_value, addr_result->ai_addr, addr_result->ai_addrlen) < 0) {
    log_and_exit(1, "connect");
  }

  freeaddrinfo(addr_result);

  return socket_value;
}

size_t read_and_save_to_buffer(tcp_client_persistence_t *tcp_client_persistence, char *buffer) {
  const size_t buffer_size = tcp_client_persistence->max_buffer_size;
  FILE *socket_file = tcp_client_persistence->socket_file;

  memset(buffer, 0, buffer_size);
  ssize_t read_size = fread(buffer, 1, buffer_size - 1, socket_file);

  log_and_exit_if_error(read_size);

  if (read_size == 0){
    tcp_client_persistence->connection_end = 1;
  }

  tcp_client_persistence->buffer_size = read_size;

  return tcp_client_persistence->buffer_size;
}

void send_request_beginning(tcp_client_persistence_t *tcp_client_persistence, const char *host, const char *resource) {
  FILE *socket_file = tcp_client_persistence->socket_file;

  int fprintf_exit_code =
    fprintf(socket_file,
      "GET %s HTTP/1.1\r\n"
      "Host: %s\r\n"
      "Connection: close\r\n",
      resource, host);

  log_and_exit_if_error(fprintf_exit_code);
}

void send_cookie(tcp_client_persistence_t *tcp_client_persistence, const char *cookie) {
  FILE *socket_file = tcp_client_persistence->socket_file;

  if (tcp_client_persistence->cookie_header_send == 0) {
    send_first_cookie(socket_file, cookie);
    tcp_client_persistence->cookie_header_send = 1;
  } else {
    send_next_cookie(socket_file, cookie);
  }
}

void send_first_cookie(FILE *socket_file, const char *cookie) {
  int fprintf_exit_code =
    fprintf(socket_file,
      "Cookie: %s",
      cookie);

  log_and_exit_if_error(fprintf_exit_code);
}

void send_next_cookie(FILE *socket_file, const char *cookie) {
  int fprintf_exit_code =
    fprintf(socket_file,
      "; %s",
      cookie);

  log_and_exit_if_error(fprintf_exit_code);
}

void end_request(tcp_client_persistence_t *tcp_client_persistence) {
  FILE *socket_file = tcp_client_persistence->socket_file;

  if (tcp_client_persistence->cookie_header_send == 1) {
    end_request_after_cookies(socket_file);
  } else {
    end_request_after_headers(socket_file);
  }
}

void end_request_after_headers(FILE *socket_file) {
  int fprintf_exit_code =
    fprintf(socket_file, "\r\n");

  log_and_exit_if_error(fprintf_exit_code);
}

void end_request_after_cookies(FILE *socket_file) {
  int fprintf_exit_code =
    fprintf(socket_file, "\r\n\r\n");

  log_and_exit_if_error(fprintf_exit_code);
}

void log_and_exit_if_error(int exit_code) {
  if (exit_code < 0) {
    log_and_exit(exit_code, "fprintf");
  }
}

int does_connection_ended(tcp_client_persistence_t *tcp_client_persistence) {
  return tcp_client_persistence->connection_end;
}

size_t get_buffer_size(tcp_client_persistence_t *tcp_client_persistence) {
  return tcp_client_persistence->buffer_size;
}

void delete_tcp_client_persistence(tcp_client_persistence_t *tcp_client_persistence) {
  fclose(tcp_client_persistence->socket_file);
  close(tcp_client_persistence->socket_value);

  free(tcp_client_persistence);
}

