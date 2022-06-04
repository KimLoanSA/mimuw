#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "err.h"

#define LINE_SIZE 1024

ssize_t read_wrapper(int s, char *line, size_t line_size,
  const char *error_mess) {
  memset(line, 0, LINE_SIZE);

  int ret = read(s, line, line_size);
  if (ret < 0) {
    syserr(error_mess);
  }

  return ret;
}

uint64_t read_int64(int s, char *line) {
  size_t line_size = 65;
  char *end_ptr;

  if (read_wrapper(s, line, line_size, "read int64") == 0) {
    syserr("read int64");
  }

  return strtoull(line, &end_ptr, 10);
}

void *handle_connection(void *s_ptr) {
  int ret, s;
  socklen_t len;
  char line[LINE_SIZE + 1], peername[LINE_SIZE + 1], peeraddr[LINE_SIZE + 1],
    file_name[LINE_SIZE + 1];
  struct sockaddr_in addr;

  s = *(int *) s_ptr;
  free(s_ptr);

  len = sizeof(addr);

  /* Któż to do nas dzwoni (adres)?  */
  ret = getpeername(s, (struct sockaddr *) &addr, &len);
  if (ret == -1) {
    syserr("getsockname");
  }

  inet_ntop(AF_INET, &addr.sin_addr, peeraddr, LINE_SIZE);
  snprintf(peername, 2 * LINE_SIZE, "%s:%d", peeraddr, ntohs(addr.sin_port));

  /* czytamy dlugosc nazwy pliku */
  ssize_t file_name_size = read_int64(s, line);

  /* czytamy nazwe pliku */
  if (read_wrapper(s, line, file_name_size, "file name read") !=
    file_name_size) {
    syserr("file name read");
  }
  strcpy(file_name, line);

  /* czytamy dlugosc pliku */
  uint64_t file_size = read_int64(s, line);

  printf("new client %s size=%lu file=%s\n", peername, file_size, file_name);

  /* tworzymy plik */
  FILE *file = fopen(file_name, "w");
  if (file == NULL) {
    syserr("file open");
  }

  sleep(1);
  uint64_t sum_of_sizes = 0;

  for (;;) {
    ret = read_wrapper(s, line, LINE_SIZE, "line read");
    if (ret == 0) {
      break;
    }

    fprintf(file, "%s", line);

    sum_of_sizes += ret;
  }

  printf("client %s has sent its file of size=%lu\n", peername, file_size);
  printf("total size of uploaded files %lu\n", sum_of_sizes);

  close(s);
  fclose(file);

  return 0;
}

int main(int argc, char *argv[]) {
  int ear, rc;
  socklen_t len;
  struct sockaddr_in server;

  if (argc != 2) {
    fatal("Usage: %s port", argv[0]);
  }
  int port = atoi(argv[1]);

  /* Tworzymy gniazdko */
  ear = socket(PF_INET, SOCK_STREAM, 0);
  if (ear == -1) {
    syserr("socket");
  }


  /* Podłączamy do centrali */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = htonl(INADDR_ANY);
  server.sin_port = htons(port);
  rc = bind(ear, (struct sockaddr *) &server, sizeof(server));
  if (rc == -1)
    syserr("bind");


  /* Każdy chce wiedzieć jaki to port */
  len = (socklen_t) sizeof(server);
  rc = getsockname(ear, (struct sockaddr *) &server, &len);
  if (rc == -1)
    syserr("getsockname");

  printf("Listening at port %d\n", (int) ntohs(server.sin_port));

  rc = listen(ear, 5);
  if (rc == -1) {
    syserr("listen");
  }

  /* No i do pracy */
  for (;;) {
    int msgsock;
    int *con;
    pthread_t t;

    msgsock = accept(ear, (struct sockaddr *) NULL, NULL);
    if (msgsock == -1) {
      syserr("accept");
    }

    /* Tylko dla tego wątku */
    con = malloc(sizeof(int));
    if (!con) {
      syserr("malloc");
    }
    *con = msgsock;

    rc = pthread_create(&t, 0, handle_connection, con);
    if (rc == -1) {
      syserr("pthread_create");
    }

    /* No przecież nie będę na niego czekał ... */
    rc = pthread_detach(t);
    if (rc == -1) {
      syserr("pthread_detach");
    }
  }
}

