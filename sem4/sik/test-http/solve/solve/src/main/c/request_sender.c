#include "request_sender.h"

#include "safe_memory.h"
#include "file_reader.h"
#include "tcp_client.h"

static void send_cookies(request_sender_persistence_t *request_sender_persistence);
static void read_and_send_cookie(file_reader_persistence_t *file_reader_persistence,
  tcp_client_persistence_t *tcp_client_persistence);

typedef struct request_sender_persistence {
  file_reader_persistence_t *file_reader_persistence;
  tcp_client_persistence_t *tcp_client_persistence;

} request_sender_persistence_t;

request_sender_persistence_t *init_request_sender(file_reader_persistence_t *file_reader_persistence,
  tcp_client_persistence_t *tcp_client_persistence) {

  request_sender_persistence_t *request_sender_persistence;
  request_sender_persistence = malloc_or_exit_on_failure(sizeof(request_sender_persistence_t));

  request_sender_persistence->tcp_client_persistence = tcp_client_persistence;
  request_sender_persistence->file_reader_persistence = file_reader_persistence;

  return request_sender_persistence;
}

void send_request(request_sender_persistence_t *request_sender_persistence, const char *host, const char *resource) {
  tcp_client_persistence_t *tcp_client_persistence = request_sender_persistence->tcp_client_persistence;

  send_request_beginning(tcp_client_persistence, host, resource);
  send_cookies(request_sender_persistence);
  end_request(tcp_client_persistence);
}

void send_cookies(request_sender_persistence_t *request_sender_persistence) {
  file_reader_persistence_t *file_reader_persistence = request_sender_persistence->file_reader_persistence;
  tcp_client_persistence_t *tcp_client_persistence = request_sender_persistence->tcp_client_persistence;

  while (has_ended(file_reader_persistence) == 0) {
    read_and_send_cookie(file_reader_persistence, tcp_client_persistence);
  }
}

void read_and_send_cookie(file_reader_persistence_t *file_reader_persistence,
  tcp_client_persistence_t *tcp_client_persistence) {
  const char *cookie = get_next_line(file_reader_persistence);

  if (has_ended(file_reader_persistence) == 0) {
    send_cookie(tcp_client_persistence, cookie);
  }

  free((void *) cookie);
}

void delete_request_sender(request_sender_persistence_t *request_sender_persistence) {
  free(request_sender_persistence);
}