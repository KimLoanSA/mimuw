#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

extern const int sys_nerr;

void syserr(const char *fmt, ...)  
{
  va_list fmt_args;

  fprintf(stderr, "ERROR: ");

  va_start(fmt_args, fmt);
  vfprintf(stderr, fmt, fmt_args);
  va_end (fmt_args);
  fprintf(stderr," (%d; %s)\n", errno, strerror(errno));
  exit(1);
}

int main ()
{
  printf("Hello from %d\n", getpid());
  if (fork() == -1) syserr("Error in fork\n");  /* powstaje nowy proces */
  printf("Goodbye from %d\n", getpid());
  return 0;  
}