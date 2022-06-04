/*
 Ten program używa poll(), aby równocześnie obsługiwać wielu klientów
 bez tworzenia procesów ani wątków.
*/

#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include "err.h"

#define TRUE 1
#define FALSE 0
#define BUF_SIZE 1024

static int finish = FALSE;
static int active_clients = 0;
static int all_server_clients = 0;
static int active_server_clients = 0;

static void catch_int (int sig);
void clean_clients_revents(struct pollfd *client);
void close_socket_if_interrupted(struct pollfd *client);
void resolve_pool(struct pollfd *client, int *client_type, char *buf);
void resolve_pool_if_count_negative();
void resolve_pool_if_count_positive(struct pollfd *client, int *client_type, char *buf);
void resolve_connection(struct pollfd *client, int *client_type, char *buf);
void create_socket_port(struct pollfd *client, const int client_index, const char *port_message, const int port);
void create_new_connection(struct pollfd *client, int *client_type, int client_index);
void receive_data_from_client(struct pollfd *client, char *buf, ssize_t rval, int *client_type, int i);
void send_control_info(struct pollfd *client, int i, char *buf, int *client_type);
void close_connection(struct pollfd *client, int *client_type, int i);
int compare_strings(const char *s1, const char *s2, const int n);

int main (int argc, char *argv[]) {
  struct pollfd client[_POSIX_OPEN_MAX];
  int client_type[_POSIX_OPEN_MAX];
  char buf[BUF_SIZE];
  struct sigaction action;
  sigset_t block_mask;

  if (argc != 3) {
    fatal("Usage: %s <port> <control port> ", argv[0]);
  }

  const int port = atoi(argv[1]);
  const int control_port = atoi(argv[2]);

  fprintf(stderr,"_POSIX_OPEN_MAX = %d\n", _POSIX_OPEN_MAX);

  /* Po Ctrl-C kończymy */
  sigemptyset (&block_mask);
  action.sa_handler = catch_int;
  action.sa_mask = block_mask;
  action.sa_flags = SA_RESTART;
  
  if (sigaction (SIGINT, &action, 0) == -1) {
    syserr("sigaction");
  }
 
  /* Inicjujemy tablicę z gniazdkami klientów, client[0] to gniazdko centrali */
  for (int i = 0; i < _POSIX_OPEN_MAX; ++i) {
    client[i].fd = -1;
    client[i].events = POLLIN;
    client[i].revents = 0;
  }

  create_socket_port(client, 0, "Socket port ", port);
  create_socket_port(client, 1, "Control port ", control_port);
 
  /* Do pracy */
  do {
    clean_clients_revents(client);
    close_socket_if_interrupted(client);
    resolve_pool(client, client_type, buf);
  } while (finish == FALSE || active_clients > 0);

  if (client[0].fd >= 0 && close(client[0].fd) < 0) {
    syserr("Closing main socket");
  }

  exit(EXIT_SUCCESS);
}

void create_socket_port(struct pollfd *client, const int client_index, const char *port_message, const int port) {
  struct sockaddr_in server;
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = htonl(INADDR_ANY);
  server.sin_port = htons(port);

  client[client_index].fd = socket(PF_INET, SOCK_STREAM, 0);
  if (client[client_index].fd == -1) {
    syserr("Opening stream socket");
  }

  if (bind(client[client_index].fd, (struct sockaddr*) &server, (socklen_t)sizeof(server)) == -1) {
    syserr("Binding stream socket");
  }

  size_t length = sizeof(server);
  if (getsockname (client[client_index].fd, (struct sockaddr*)&server,
    (socklen_t*)&length) == -1) {
    syserr("Getting socket name");
  }

  printf("%s #%u\n", port_message, (unsigned)ntohs(server.sin_port));

  if (listen(client[client_index].fd, 5) == -1) {
    syserr("Starting to listen");
  }
}

void catch_int(int sig) {
  finish = TRUE;
  fprintf(stderr, "Signal %d catched. No new connections will be accepted.\n", sig);
}

void resolve_pool(struct pollfd *client, int *client_type, char *buf) {
  int descriptors_count = poll(client, _POSIX_OPEN_MAX, -1);

  if (descriptors_count == -1) {
    resolve_pool_if_count_negative();
  }
  else if (descriptors_count > 0) {
    resolve_pool_if_count_positive(client, client_type, buf);
  } else {
    fprintf(stderr, "Do something else\n");
  }
}

void resolve_pool_if_count_positive(struct pollfd *client, int *client_type, char *buf) {
  create_new_connection(client, client_type, 0);
  create_new_connection(client, client_type, 1);

  resolve_connection(client, client_type, buf);
}

void create_new_connection(struct pollfd *client, int *client_type, int client_index) {
  if (finish == FALSE && (client[client_index].revents & POLLIN)) {
    int msgsock = accept(client[client_index].fd, (struct sockaddr*)0, (socklen_t*)0);

    if (msgsock == -1) {
      syserr("accept");
    }
    else {
      int i;
      for (i = 2; i < _POSIX_OPEN_MAX; ++i) {
        if (client[i].fd == -1) {
          fprintf(stderr, "Received new connection (%d)\n", i);

          client[i].fd = msgsock;
          client[i].events = POLLIN;
          client_type[i] = client_index;
          active_clients++;

          if (client_index == 0) {
            active_server_clients++;
            all_server_clients++;
          }

          break;
        }
      }
      if (i >= _POSIX_OPEN_MAX) {
        fprintf(stderr, "Too many clients\n");
        if (close(msgsock) < 0) {
          syserr("close");
        }
      }
    }
  }
}

void resolve_connection(struct pollfd *client, int *client_type, char *buf) {
  for (int i = 2; i < _POSIX_OPEN_MAX; ++i) {
    if (client[i].fd != -1 && (client[i].revents & (POLLIN | POLLERR))) {
      ssize_t rval = read(client[i].fd, buf, BUF_SIZE);
      if (rval < 0) {
        fprintf(stderr, "Reading message (%d, %s)\n", errno, strerror(errno));
        if (close(client[i].fd) < 0) {
          syserr("close");
        }
        client[i].fd = -1;

        if (client_type[i] == 0) {
          active_server_clients--;
        }

        client_type[i] = -1;
        active_clients--;
      } else if (rval == 0) {
        close_connection(client, client_type, i);
      } else {
        receive_data_from_client(client, buf, rval, client_type, i);
      }
    }
  }
}

void receive_data_from_client(struct pollfd *client, char *buf, ssize_t rval, int *client_type, int i) {
  if (client_type[i] == 0) {
    printf("-->%.*s\n", (int) rval, buf);
  } else if (client_type[i] == 1) {
    send_control_info(client, i, buf, client_type);
  } else {
    printf("cos nie dziala\n");
  }
}

void send_control_info(struct pollfd *client, int i, char *buf, int *client_type) {
  char command[] = "count";

  if (strlen(buf) == 7 && compare_strings(command, buf, 5)) {
    char str[BUF_SIZE];
    sprintf(str, "Number of active clients: %d\nTotal number of clients: %d\n", active_server_clients, all_server_clients);
    write(client[i].fd, str, strlen(str));
  }

  close_connection(client, client_type, i);
}

int compare_strings(const char *s1, const char *s2, const int n) {
  for (int i = 0; i < n; i++) {
    if (s1[i] != s2[i]) {
      return 0;
    }
  }

  return 1;
}

void close_connection(struct pollfd *client, int *client_type, int i) {
  fprintf(stderr, "Ending connection\n");
  if (close(client[i].fd) < 0) {
    syserr("close");
  }
  client[i].fd = -1;

  if (client_type[i] == 0) {
    active_server_clients--;
  }

  client_type[i] = -1;
  active_clients--;
}

void resolve_pool_if_count_negative() {
  if (errno == EINTR) {
    fprintf(stderr, "Interrupted system call\n");
  } else {
    syserr("poll");
  }
}

void close_socket_if_interrupted(struct pollfd *client) {
  if (finish == TRUE && client[0].fd >= 0) {
    if (close(client[0].fd) < 0) {
      syserr("close");
    }

    client[0].fd = -1;
  }
}

void clean_clients_revents(struct pollfd *client) {
  for (int i = 0; i < _POSIX_OPEN_MAX; ++i) {
    client[i].revents = 0;
  }
}

