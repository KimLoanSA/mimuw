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

#define BUF_SIZE 1024

int main (int argc, char *argv[])
{
  int read_dsc, buf_len;
  char buf[BUF_SIZE];
  
  // if (argc != 2)
    // fatal("Usage: %s <read_fd>\n", argv[0]);
  
  read_dsc = atoi(argv[1]);
  printf("Reading data from descriptor %d\n", read_dsc);

  if ((buf_len = read(read_dsc, buf, BUF_SIZE - 1)) == -1)
    syserr("Error in read\n");;
  
  buf[buf_len < BUF_SIZE - 1 ? buf_len : BUF_SIZE - 1] = '\0';
  /* ustawienie znaku końca napisu za wczytanymi danymi */
  
  // if (buf_len == 0)                         
  //   fatal("Unexpected end-of-file\n");
  // else      
    printf("Read %d byte(s): \"%s\"\n", buf_len, buf);

  exit(0);
}
