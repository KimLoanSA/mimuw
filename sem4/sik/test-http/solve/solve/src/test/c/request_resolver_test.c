#include "../../../../docker-test/src/test/c/interfaces/request_resolver_test.h"

#include "../../main/c/request_sender.h"
#include "../../main/c/address_resolver.h"

#include <stdio.h>

void send_request_test(const char *address, const char *file_name) {
  const char *host = get_host(address);
  const char *resource = get_resource(address);

  file_reader_persistence_t *file_reader_persistence;
  file_reader_persistence = init_file_reader(file_name);

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence("www.mimuw.edu.pl", "80", 1);

  request_sender_persistence_t *request_sender_persistence;
  request_sender_persistence = init_request_sender(file_reader_persistence, tcp_client_persistence);

  send_request(request_sender_persistence, host, resource);

  delete_request_sender(request_sender_persistence);
  delete_file_reader(file_reader_persistence);
  delete_tcp_client_persistence(tcp_client_persistence);
}
