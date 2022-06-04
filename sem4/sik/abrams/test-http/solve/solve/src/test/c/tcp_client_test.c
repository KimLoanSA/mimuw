#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "../../main/c/tcp_client.h"

int main() {

  printf("==================================\n");
  printf("TCP CLIENT TESTS\n");
  printf("==================================\n");

  char *input[] = {
    "abc",
    "xyz",
    "123"
  };

  char buffer[10];

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 3);

  assert(read_and_save_to_buffer(tcp_client_persistence, buffer) == 3);
  assert(strcmp(buffer, "abc") == 0);
  assert(does_connection_ended(tcp_client_persistence) == 0);
  assert(read_and_save_to_buffer(tcp_client_persistence, buffer) == 3);
  assert(strcmp(buffer, "xyz") == 0);
  assert(does_connection_ended(tcp_client_persistence) == 0);
  assert(read_and_save_to_buffer(tcp_client_persistence, buffer) == 3);
  assert(strcmp(buffer, "123") == 0);
  assert(does_connection_ended(tcp_client_persistence) == 1);

  send_request_beginning(tcp_client_persistence, "host", "/daj");
  printf("\nGET /daj HTTP/1.1\n"
         "Host: xd\n"
         "User-Agent: testhttp_raw\n"
         "Connection: close <- expected\n\n");

  send_cookie(tcp_client_persistence, "cookie1=hej");
  printf("Cookie: cookie1=hej <- expected\n\n");

  delete_tcp_client_persistence(tcp_client_persistence);

  printf("\nall asserts passed\n");
  printf("==================================\n\n");

  return 0;
}