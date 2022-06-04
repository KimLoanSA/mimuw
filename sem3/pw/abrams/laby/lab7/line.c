#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

#define NR_PROC 5

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

void hey(int n) {
  if (n > 0) {

    pid_t pid = fork();

    if (pid == -1) {
      syserr("Error in fork\n");
    } else if (pid == 0) {
      printf("parent: %d me %d\n", getppid(), getpid());
      hey(n - 1);
    } else {
      if (wait(0) == -1)
        syserr("Error in wait\n");
    }
  }
}

int main ()
{
 
  hey(NR_PROC);

  return 0;
  
}