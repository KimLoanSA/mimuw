#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/ioctl.h>
#include <minix/ioctl.h>
#include <sys/ioc_dfa.h>

int main(int argc, char** argv) {
  int fd, a;
  char tr[3];

  if ((fd = open("/dev/dfa", O_RDONLY)) < 0)
    exit(1);

  tr[0] = 0;
  tr[2] = 1;

  for (a = SCHAR_MIN; a <= SCHAR_MAX; a++) {
    tr[1] = (char) a;
    if (ioctl(fd, DFAIOCADD, tr) < 0)
      exit(1);
  }

  tr[0] = 1;
  if (ioctl(fd, DFAIOCACCEPT, tr) < 0)
    exit(1);
}
