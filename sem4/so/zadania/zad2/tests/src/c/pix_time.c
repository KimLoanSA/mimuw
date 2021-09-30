#include "pix.h"

#include <stdio.h>

void pixtime(uint64_t clock_tick) {
  fprintf(stderr, "%016lX\n", clock_tick);
}
