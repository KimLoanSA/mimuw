#ifndef MIMUW_S4_SIK_TESTHTTP_TCP_CLIENT_H
#define MIMUW_S4_SIK_TESTHTTP_TCP_CLIENT_H

#include <stdlib.h>

typedef struct tcp_client_persistence tcp_client_persistence_t;

tcp_client_persistence_t *init_tcp_client_persistence(const char *host, const char*port, size_t buffer_size);
size_t read_and_save_to_buffer(tcp_client_persistence_t *tcp_client_persistence, char *buffer);
void send_request_beginning(tcp_client_persistence_t *tcp_client_persistence, const char *host, const char *resource);
void send_cookie(tcp_client_persistence_t *tcp_client_persistence, const char *cookie);
void end_request(tcp_client_persistence_t *tcp_client_persistence);
int does_connection_ended(tcp_client_persistence_t *tcp_client_persistence);
size_t get_buffer_size(tcp_client_persistence_t *tcp_client_persistence);
void delete_tcp_client_persistence(tcp_client_persistence_t *tcp_client_persistence);

#endif //MIMUW_S4_SIK_TESTHTTP_TCP_CLIENT_H