#include "../../../../docker-test/src/test/c/interfaces/response_resolver_test.h"

#include <stdio.h>

#include "../../main/c/response_resolver.h"
#include "../../main/c/tcp_client.h"

void report_for_response_test(char **response_parts, size_t number_of_response_parts, size_t response_part_size) {
  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(response_part_size, response_parts, number_of_response_parts);

  response_resolver_persistence_t *response_resolver_persistence;
  response_resolver_persistence = init_response_resolver_persistence(tcp_client_persistence);

  report_response(response_resolver_persistence);

  delete_response_resolver_persistence(response_resolver_persistence);
  delete_tcp_client_persistence(tcp_client_persistence);
}