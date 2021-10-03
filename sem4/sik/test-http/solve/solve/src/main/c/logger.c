#include "logger.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void log_and_exit(const int exit_code, const char *message, ...){
  va_list message_args;
  va_start(message_args, message);

  fprintf(stderr, "ERROR: ");
  vfprintf(stderr, message, message_args);

  va_end(message_args);

  fprintf(stderr, "\n");

  exit(exit_code);
}