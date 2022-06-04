#include <stdio.h>

#include "program_arguments_validator.h"
#include "file_reader.h"
#include "address_resolver.h"
#include "response_resolver.h"
#include "request_sender.h"
#include "tcp_client.h"

int main(int argc, char *argv[]) {
  validate_program_arguments(argc, (const char **) argv);

  file_reader_persistence_t *file_reader_persistence;
  tcp_client_persistence_t *tcp_client_persistence;
  request_sender_persistence_t *request_sender_persistence;
  response_resolver_persistence_t *response_resolver_persistence;

  const char *connection_address = argv[1];
  const char *file_name = argv[2];
  const char *request_address = argv[3];

  const char *connection_host = get_host_without_port(connection_address);
  const char *connection_port = get_port(connection_address);

  const char *request_host = get_host(request_address);

  const char *request_resource = get_resource(request_address);
  const size_t buffer_size = 1024;

  tcp_client_persistence = init_tcp_client_persistence(connection_host, connection_port, buffer_size);
  file_reader_persistence = init_file_reader(file_name);

  request_sender_persistence = init_request_sender(file_reader_persistence, tcp_client_persistence);
  send_request(request_sender_persistence, request_host, request_resource);
  delete_request_sender(request_sender_persistence);

  response_resolver_persistence = init_response_resolver_persistence(tcp_client_persistence);
  report_response(response_resolver_persistence);
  delete_response_resolver_persistence(response_resolver_persistence);

  delete_file_reader(file_reader_persistence);
  delete_tcp_client_persistence(tcp_client_persistence);

  free((void *) connection_host);
  free((void *) connection_port);

  free((void *) request_host);
  free((void *) request_resource);

  return 0;
}