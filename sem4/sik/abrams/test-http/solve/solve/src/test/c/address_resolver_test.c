#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "../../main/c/address_resolver.h"

int main() {

  printf("==================================\n");
  printf("ADDRESS RESOLVER TESTS\n");
  printf("==================================\n");

  const char *address_ok_1 = "www.mimuw.edu.pl:1234";
  assert(get_port_long(address_ok_1) == 1234);
  assert(strcmp(get_port(address_ok_1), "1234") == 0);

  const char *address_ok_2 = "www.mimuw.edu.pl:1";
  assert(get_port_long(address_ok_2) == 1);
  assert(strcmp(get_port(address_ok_2), "1") == 0);

  const char *address_bad_1 = "www.mimuw.edu.pl:";
  assert(get_port_long(address_bad_1) == -1);
  assert(strcmp(get_port(address_bad_1), "") == 0);

  const char *address_bad_2 = "www.mimuw.edu.pl";
  assert(get_port_long(address_bad_2) == -1);
  assert(strcmp(get_port(address_bad_2), "") == 0);


  const char *host_without_port_address_1 = "www.mimuw.edu.pl:8080";
  assert(strcmp(get_host_without_port(host_without_port_address_1), "www.mimuw.edu.pl") == 0);

  const char *host_without_port_address_2 = "www.mimuw.edu.pl:80";
  assert(strcmp(get_host_without_port(host_without_port_address_2), "www.mimuw.edu.pl") == 0);


  const char *host_address_1 = "http://www.mimuw.edu.pl/";
  assert(strcmp(get_host(host_address_1), "www.mimuw.edu.pl") == 0);

  const char *host_address_2 = "http://www.mimuw.edu.pl:8080/plik";
  assert(strcmp(get_host(host_address_2), "www.mimuw.edu.pl:8080") == 0);

  const char *host_address_3 = "http://www.mimuw.edu.pl:8080?plik";
  assert(strcmp(get_host(host_address_3), "www.mimuw.edu.pl:8080") == 0);

  const char *host_address_4 = "http://www.mimuw.edu.pl:8080#plik";
  assert(strcmp(get_host(host_address_4), "www.mimuw.edu.pl:8080") == 0);


  const char *resource_address_1 = "https://www.mimuw.edu.pl/plik";
  assert(strcmp(get_resource(resource_address_1), "/plik") == 0);

  const char *resource_address_2 = "http://www.mimuw.edu.pl:8080/plik";
  assert(strcmp(get_resource(resource_address_2), "/plik") == 0);

  const char *resource_address_3 = "http://www.mimuw.edu.pl:8080?plik";
  assert(strcmp(get_resource(resource_address_3), "/?plik") == 0);

  const char *resource_address_4 = "http://www.mimuw.edu.pl:8080#plik";
  assert(strcmp(get_resource(resource_address_4), "/") == 0);

  const char *resource_address_5 = "http://www.mimuw.edu.pl:8080/plik#plik2";
  assert(strcmp(get_resource(resource_address_5), "/plik") == 0);

  const char *resource_address_6 = "http://www.mimuw.edu.pl:8080?plik#pilk2";
  assert(strcmp(get_resource(resource_address_6), "/?plik") == 0);


  printf("\nall asserts passed\n");
  printf("==================================\n\n");

  return 0;
}