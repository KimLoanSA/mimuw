#include <stdio.h>
#include <assert.h>

#include "../../main/c/buffered_reader.h"
#include "../../main/c/tcp_client.h"

void get_chars_test();
void skip_n_chars_test();
void print_to_string_1_test();
void print_to_string_2_test();
void print_to_string_3_test();
void read_until_end_test();

int main() {

  printf("==================================\n");
  printf("BUFFERED READER TESTS\n");
  printf("==================================\n");

  get_chars_test();

  skip_n_chars_test();

  print_to_string_1_test();
  print_to_string_2_test();
  print_to_string_3_test();

  read_until_end_test();

  printf("\nall asserts passed\n");
  printf("==================================\n\n");

  return 0;
}

void get_chars_test() {
  char *input[] = {
    "abc",
    "xyz",
  };

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 2);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);

  assert(get_one_char(buffered_reader_persistence) == 'a');
  assert(get_one_char(buffered_reader_persistence) == 'b');
  assert(get_one_char(buffered_reader_persistence) == 'c');

  assert(get_one_char(buffered_reader_persistence) == 'x');
  assert(get_one_char(buffered_reader_persistence) == 'y');
  assert(get_one_char(buffered_reader_persistence) == 'z');

  delete_tcp_client_persistence(tcp_client_persistence);
  delete_buffered_reader_persistence(buffered_reader_persistence);
}

void skip_n_chars_test() {
  char *input[] = {
    "abc",
    "xyz",
  };

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 2);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);

  skip_n_next_chars(buffered_reader_persistence, 4);

  assert(get_one_char(buffered_reader_persistence) == 'y');

  delete_tcp_client_persistence(tcp_client_persistence);
  delete_buffered_reader_persistence(buffered_reader_persistence);
}

void print_to_string_1_test() {
  char *input[] = {
    "abc",
    "xyz",
  };

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 2);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);

  print_all_to_given_string(buffered_reader_persistence, "xy");
  printf("\nabc <- expected\n");

  delete_tcp_client_persistence(tcp_client_persistence);
  delete_buffered_reader_persistence(buffered_reader_persistence);
}

void print_to_string_2_test() {
  char *input[] = {
    "abc",
    "xyz",
  };

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 2);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);

  print_all_to_given_string(buffered_reader_persistence, "c");
  printf("\nab <- expected\n");

  delete_tcp_client_persistence(tcp_client_persistence);
  delete_buffered_reader_persistence(buffered_reader_persistence);
}

void print_to_string_3_test() {
  char *input[] = {
    "abc",
    "xyz",
  };

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 2);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);

  print_all_to_given_string(buffered_reader_persistence, "z");
  printf("\nabcxy <- expected\n");

  delete_tcp_client_persistence(tcp_client_persistence);
  delete_buffered_reader_persistence(buffered_reader_persistence);
}

void read_until_end_test() {
  char *input[] = {
    "abc",
    "xyz",
  };

  tcp_client_persistence_t *tcp_client_persistence;
  tcp_client_persistence = init_tcp_client_persistence_test(3, input, 2);

  buffered_reader_persistence_t *buffered_reader_persistence;
  buffered_reader_persistence = init_buffered_reader_persistence(tcp_client_persistence);

  get_one_char(buffered_reader_persistence);
  get_one_char(buffered_reader_persistence);
  assert(read_until_end_and_give_size(buffered_reader_persistence) == 4);

  delete_tcp_client_persistence(tcp_client_persistence);
  delete_buffered_reader_persistence(buffered_reader_persistence);
}