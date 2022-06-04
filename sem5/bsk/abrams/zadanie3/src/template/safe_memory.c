#include "safe_memory.h"

#include <stdio.h>

void *malloc_or_exit_on_failure(size_t size) {
  void *pointer = malloc(size);

  if (pointer == NULL) {
    fprintf(stderr, "malloc fail");
  }

  return pointer;
}
