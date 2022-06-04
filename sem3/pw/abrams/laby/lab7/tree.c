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

int main ()
{
  pid_t pid;
  int i; 

  /* tworzenie procesów potomnych */
  for (i = 1; i <= NR_PROC; i++)
    switch (pid = fork()) {
      case -1: 
        syserr("Error in fork\n");

      case 0: /* proces potomny */
 
        printf("I am a child and my pid is %d\n", getpid());
        return 0;
    
    default: /* proces macierzysty */
      printf("I am a parent and my pid is %d\n", getpid());

      if (wait(0) == -1) 
        syserr("Error in wait\n");
      exit(0);
    } 
         
  if (wait(0) == -1) 
        syserr("Error in wait\n");
  return 0;
  
}
