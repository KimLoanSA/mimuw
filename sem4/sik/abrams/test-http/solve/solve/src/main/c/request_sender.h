#ifndef MIMUW_S4_SIK_TESTHTTP_REQUEST_SENDER_H
#define MIMUW_S4_SIK_TESTHTTP_REQUEST_SENDER_H

#include "file_reader.h"
#include "tcp_client.h"

typedef struct request_sender_persistence request_sender_persistence_t;

request_sender_persistence_t *init_request_sender(file_reader_persistence_t *file_reader_persistence,
  tcp_client_persistence_t *tcp_client_persistence);
void send_request(request_sender_persistence_t *request_sender_persistence, const char *host, const char *resource);
void delete_request_sender(request_sender_persistence_t *request_sender_persistence);

#endif //MIMUW_S4_SIK_TESTHTTP_REQUEST_SENDER_H
