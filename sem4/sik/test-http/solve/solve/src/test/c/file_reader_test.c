#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../../main/c/file_reader.h"

int main() {

  printf("==================================\n");
  printf("FILE READER TESTS\n");
  printf("==================================\n");

  file_reader_persistence_t *file_reader_persistence;
  file_reader_persistence = init_file_reader("src/test/resources/cookies/ciasteczka_test.txt");

  const char *test_1 = get_next_line(file_reader_persistence);
  assert(strcmp(test_1, "ciasteczko1=hej") == 0);
  assert(has_ended(file_reader_persistence) == 0);
  free(test_1);

  const char *test_2 = get_next_line(file_reader_persistence);
  assert(strcmp(test_2, "ciasteczko2=wszystkim") == 0);
  assert(has_ended(file_reader_persistence) == 0);
  free(test_2);

  const char *test_3 = get_next_line(file_reader_persistence);
  assert(has_ended(file_reader_persistence) == 1);
  free(test_3);

  delete_file_reader(file_reader_persistence);

  printf("\nall asserts passed\n");
  printf("==================================\n\n");

  return 0;
}