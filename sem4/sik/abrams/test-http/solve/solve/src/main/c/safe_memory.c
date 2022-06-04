#include "safe_memory.h"

#include "logger.h"

void *malloc_or_exit_on_failure(size_t size) {
  void *pointer = malloc(size);

  if (pointer == NULL) {
    log_and_exit(1, "malloc");
  }

  return pointer;
}
