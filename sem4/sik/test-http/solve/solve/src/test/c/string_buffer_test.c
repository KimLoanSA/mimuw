#include <assert.h>
#include <stdio.h>

#include "../../main/c/string_buffer.h"

void basic_adding_1_test();
void basic_adding_2_test();
void basic_adding_3_test();
void wildcard_test();
void print_1_test();
void print_2_test();
void print_first_test();

int main() {

  printf("==================================\n");
  printf("STRING BUFFER TESTS\n");
  printf("==================================\n");

  basic_adding_1_test();
  basic_adding_2_test();
  basic_adding_3_test();

  wildcard_test();

  print_1_test();
  print_2_test();

  print_first_test();

  printf("\nall asserts passed\n");
  printf("==================================\n\n");

  return 0;
}

void basic_adding_1_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(5);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');

  assert(does_contain_string(string_buffer_persistence, "c") > 0);
  assert(does_contain_string(string_buffer_persistence, "bc") > 0);
  assert(does_contain_string(string_buffer_persistence, "abc") > 0);
  assert(does_contain_string(string_buffer_persistence, "ab") == 0);
  assert(does_contain_string(string_buffer_persistence, "a") == 0);

  delete_string_buffer_persistence(string_buffer_persistence);
}

void basic_adding_2_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(3);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');

  assert(does_contain_string(string_buffer_persistence, "c") > 0);
  assert(does_contain_string(string_buffer_persistence, "bc") > 0);
  assert(does_contain_string(string_buffer_persistence, "abc") > 0);
  assert(does_contain_string(string_buffer_persistence, "ab") == 0);
  assert(does_contain_string(string_buffer_persistence, "a") == 0);

  delete_string_buffer_persistence(string_buffer_persistence);
}

void basic_adding_3_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(3);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');
  consume_char(string_buffer_persistence, 'd');

  assert(does_contain_string(string_buffer_persistence, "d") > 0);
  assert(does_contain_string(string_buffer_persistence, "cd") > 0);
  assert(does_contain_string(string_buffer_persistence, "bcd") > 0);
  assert(does_contain_string(string_buffer_persistence, "abc") == 0);
  assert(does_contain_string(string_buffer_persistence, "ab") == 0);
  assert(does_contain_string(string_buffer_persistence, "c") == 0);
  assert(does_contain_string(string_buffer_persistence, "a") == 0);

  assert(does_contain_string_non_case_sensitive(string_buffer_persistence, "BcD") > 0);

  delete_string_buffer_persistence(string_buffer_persistence);
}

void wildcard_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(5);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');

  assert(does_contain_string(string_buffer_persistence, "*") > 0);
  assert(does_contain_string(string_buffer_persistence, "b*") > 0);
  assert(does_contain_string(string_buffer_persistence, "**") > 0);
  assert(does_contain_string(string_buffer_persistence, "***") > 0);
  assert(does_contain_string(string_buffer_persistence, "*b") == 0);

  delete_string_buffer_persistence(string_buffer_persistence);
}

void print_1_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(5);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');

  print_last_n_chars(string_buffer_persistence, 3);
  printf("\nabc <- expected \n");
  print_last_n_chars(string_buffer_persistence, 1);
  printf("\nc <- expected \n");
  print_last_n_chars(string_buffer_persistence, 2);
  printf("\nbc <- expected \n");

  delete_string_buffer_persistence(string_buffer_persistence);
}

void print_2_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(3);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');
  consume_char(string_buffer_persistence, 'd');

  print_last_n_chars(string_buffer_persistence, 3);
  printf("\nbcd <- expected \n");
  print_last_n_chars(string_buffer_persistence, 1);
  printf("\nd <- expected \n");
  print_last_n_chars(string_buffer_persistence, 2);
  printf("\ncd <- expected \n");

  delete_string_buffer_persistence(string_buffer_persistence);
}

void print_first_test() {
  string_buffer_persistence_t *string_buffer_persistence;
  string_buffer_persistence = init_string_buffer_persistence(3);

  consume_char(string_buffer_persistence, 'a');
  consume_char(string_buffer_persistence, 'b');
  consume_char(string_buffer_persistence, 'c');

  print_first_char(string_buffer_persistence);
  printf("\na <- expected \n");

  consume_char(string_buffer_persistence, 'd');

  print_first_char(string_buffer_persistence);
  printf("\nb <- expected \n");

  delete_string_buffer_persistence(string_buffer_persistence);
}