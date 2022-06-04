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
  pid_t pid;

  switch (pid = fork()) {                     /* powstaje nowy proces */

    case -1:                                  /* błąd funkcji fork() */
      syserr("Error in fork\n");              

    case 0:                                   /* proces potomny */

      printf("I am a child and my pid is %d\n", getpid());
      printf("I am a child and fork() return value is %d\n", pid);

      return 0;                               /* proces potomny kończy */

    default:                                  /* proces macierzysty */

      printf("I am a parent and my pid is %d\n", getpid());
      printf("I am a parent and fork() return value is %d\n", pid);

      if (wait(0) == -1) syserr("Error in wait\n");
					      /* czeka na zakończenie procesu potomnego */

      return 0;
  }
}