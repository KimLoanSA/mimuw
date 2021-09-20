#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

void syserr(const char *fmt, ...) {
  va_list fmt_args;

  fprintf(stderr, "ERROR: ");

  va_start(fmt_args, fmt);
  vfprintf(stderr, fmt, fmt_args);
  va_end (fmt_args);
  fprintf(stderr," (%d; %s)\n", errno, strerror(errno));
  exit(1);
}

char message[] = "Hello from your parent!\n";

int main (int argc, char *argv[])
{

  printf("XD%d\n", argc);
  int pipe_dsc[2];
  
  if (pipe(pipe_dsc) == -1) 
    syserr("Error in pipe\n");

  switch (fork ()) {
    case -1: 
      syserr("Error in fork\n");

    case 0:
      if (close (0) == -1)            syserr("Error in child, close (0)\n");
      if (dup (pipe_dsc [0]) != 0)    syserr("Error in child, dup (pipe_dsc [0])\n");
      if (close (pipe_dsc [0]) == -1) syserr("Error in child, close (pipe_dsc [0])\n");
      if (close (pipe_dsc [1]) == -1) syserr("Error in child, close (pipe_dsc [1])\n");

      if (argc >= 2) {
        /* argv + 1 to argv bez pierwszego elementu - nazwy programu parent_dup */
        printf("%s %s\n", argv[1], (char*)(argv + 1));
        execvp(argv[1], argv + 1);
        syserr("Error in execvp\n");
      }
      exit (0);

    default:
      if (close (pipe_dsc [0]) == -1) 
        syserr("Error in parent, close (pipe_dsc [0])\n");


      if (write (pipe_dsc [1], message, sizeof(message) - 1) != sizeof(message) - 1)
        syserr("Error in write\n");

      if (close (pipe_dsc [1]) == -1) 
        syserr("Error in parent, close (pipe_dsc [1])\n");

      if (wait (0) == -1) 
        syserr("Error in wait\n");

      exit (0);
  } 
}
