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

char message[] = "Hello from your parent!";

int main()
{
  int pipe_dsc[2];
  char pipe_read_dsc_str[10];
  
  if (pipe(pipe_dsc) == -1) syserr("Error in pipe\n");
  
  switch (fork()) {
    case -1:
      syserr("Error in fork\n");
    
    case 0: 
      if (close(pipe_dsc[1]) == -1) syserr("Error in close(pipe_dsc[1])\n");
      
      sprintf(pipe_read_dsc_str, "%d", pipe_dsc[0]);
      execl("./child-pipe", "child-pipe", pipe_read_dsc_str, NULL);
      syserr("Error in execl\n");
      
    default:
      if (close(pipe_dsc[0]) == -1) syserr("Error in close(pipe_dsc[0])\n");
      
      if (write(pipe_dsc[1], message, sizeof(message)) != sizeof(message))
	syserr("Error in write\n");
      
      if (wait(0) == -1)
	      syserr("Error in wait\n");
      
      exit(0);
  }
}
