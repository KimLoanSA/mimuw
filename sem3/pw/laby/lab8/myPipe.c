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

int main (int argc, char *argv[]) {

  int pipe_dsc[2];
  char pipe_read_dsc_str[100];

  if (pipe(pipe_dsc) == -1) 
    syserr("Error in pipe\n");

  for (int i = 1; i < argc; i++) {

    switch (fork ()) {
    case -1: 
      syserr("Error in fork\n");

    case 0:
      if (close (0) == -1)            syserr("Error in child, close (0)\n");
      if (dup (pipe_dsc [0]) != 0)    syserr("Error in child, dup (pipe_dsc [0])\n");
      if (close (pipe_dsc [0]) == -1) syserr("Error in child, close (pipe_dsc [0])\n");
      if (close (pipe_dsc [1]) == -1) syserr("Error in child, close (pipe_dsc [1])\n");

      sprintf(pipe_read_dsc_str, "%d", pipe_dsc[0]);
      char** x = argv + i;
      x++;
      *x = pipe_read_dsc_str;
      x++;
      *x = 0;
      execvp(argv[i], x);
      syserr("Error in execvp\n");
    
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
}

