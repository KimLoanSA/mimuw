#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "err.h"

#define BUFFER_SIZE      1024

void write_wrapper(const int sock, const char *buffer, const size_t buffer_size,
  const char *error_mess) {
  if (write(sock, buffer, buffer_size) < 0) {
    syserr(error_mess);
  }
}

void write_int64(const int sock, uint64_t number) {
  const size_t int_buffer_size = 65;
  char number_str[int_buffer_size];

  snprintf (number_str, int_buffer_size, "%064lu", number);
  write_wrapper(sock, number_str, int_buffer_size, "file name size");
}

int64_t file_size(FILE *file) {
  fseek(file, 0L, SEEK_END);
  int64_t file_size = ftell(file);
  fseek(file, 0, SEEK_SET);

  return file_size;
}

int main(int argc, char *argv[]) {
  int rc;
  int sock;
  struct addrinfo addr_hints, *addr_result;
  char line[BUFFER_SIZE];

  if (argc != 4) {
    fatal("Usage: %s host port file", argv[0]);
  }

  char *file_name = argv[3];
  size_t file_name_size = strlen(file_name);

  FILE *file = fopen(file_name, "r");
  if (file == NULL) {
    syserr("file open");
  }

  sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (sock < 0) {
    syserr("socket");
  }

  /* Trzeba się dowiedzieć o adres internetowy serwera. */
  memset(&addr_hints, 0, sizeof(struct addrinfo));
  addr_hints.ai_flags = 0;
  addr_hints.ai_family = AF_INET;
  addr_hints.ai_socktype = SOCK_STREAM;
  addr_hints.ai_protocol = IPPROTO_TCP;

  rc = getaddrinfo(argv[1], argv[2], &addr_hints, &addr_result);
  if (rc != 0) {
    fprintf(stderr, "rc=%d\n", rc);
    syserr("getaddrinfo: %s", gai_strerror(rc));
  }

  if (connect(sock, addr_result->ai_addr, addr_result->ai_addrlen) != 0) {
    syserr("connect");
  }
  freeaddrinfo(addr_result);

  /* wysylanie rozmiaru nazwy pliku */
  /* zakladamy ze nazwa pliku ma mniej niz `BUFFER_SIZE`znakow!!!! */
  write_int64(sock, file_name_size);

  /* wysylanie nazwy pliku */
  write_wrapper(sock, file_name, file_name_size, "file name");

  /* wysylanie rozmiaru pliku */
  uint64_t f_size = file_size(file);
  write_int64(sock, f_size);

  for (uint64_t buf = 0; buf < f_size; buf += BUFFER_SIZE) {
    memset(line, 0, BUFFER_SIZE);
    fread(line, 1, BUFFER_SIZE, file);

    write_wrapper(sock, line, strlen(line), "writing on stream socket");
  }

  if (close(sock) < 0) {
    syserr("closing stream socket");
  }

  return 0;
}

