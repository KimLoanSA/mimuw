#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#include "err.h"

#define BUFFER_SIZE   2000
#define QUEUE_LENGTH     5
#define INITIALS "MA:"
#define INITIALS_LENGTH  3
#define BUFFER_INI_LENGTH BUFFER_SIZE + INITIALS_LENGTH

int main(int argc, char *argv[]) {
  if (argc > 2) {
    syserr("Usage ./echo-server [file]");
  }

  int write_to_file = argc > 1;
  FILE *file;
  int port = 1327;

  int sock, msg_sock;
  struct sockaddr_in server_address;
  struct sockaddr_in client_address;
  socklen_t client_address_len;

  if (write_to_file == 1) {
    file = fopen(argv[1], "w");
    if (file == NULL) {
      syserr("file open");
    }
  }

  char buffer[BUFFER_SIZE], buffer_ini[BUFFER_INI_LENGTH];
  ssize_t len, snd_len, ini_len;

  sock = socket(PF_INET, SOCK_STREAM, 0); // creating IPv4 TCP socket
  if (sock < 0)
    syserr("socket");
  // after socket() call; we should close(sock) on any execution path;
  // since all execution paths exit immediately, sock would be closed when program terminates

  server_address.sin_family = AF_INET; // IPv4
  server_address.sin_addr.s_addr = htonl(
    INADDR_ANY); // listening on all interfaces
  server_address.sin_port = htons(port); // listening on port PORT_NUM

  // bind the socket to a concrete address
  if (bind(sock, (struct sockaddr *) &server_address, sizeof(server_address)) <
    0)
    syserr("bind");

  // switch to listening (passive open)
  if (listen(sock, QUEUE_LENGTH) < 0)
    syserr("listen");

  printf("accepting client connections on port %hu\n",
    ntohs(server_address.sin_port));
  for (;;) {
    client_address_len = sizeof(client_address);
    // get client connection from the socket
    msg_sock = accept(sock, (struct sockaddr *) &client_address,
      &client_address_len);
    if (msg_sock < 0)
      syserr("accept");
    do {
      len = read(msg_sock, buffer, sizeof(buffer));
      if (len < 0)
        syserr("reading from client socket");
      else {
        if (write_to_file == 0) {
          printf("read from socket: %zd bytes\n", len);
        } else {
          fprintf(file, "read from socket: %zd bytes\n", len);
          fflush(file);
        }
        memset(buffer_ini, 0, BUFFER_INI_LENGTH);
        strcat(buffer_ini, INITIALS);
        strcat(buffer_ini, buffer);
        ini_len = len + INITIALS_LENGTH;
        snd_len = write(msg_sock, buffer_ini, ini_len);
        if (snd_len != ini_len)
          syserr("writing to client socket");
      }
    } while (len > 0);
    printf("ending connection\n");
    if (close(msg_sock) < 0)
      syserr("close");
  }
  if (write_to_file) {
    fclose(file);
  }
  return 0;
}