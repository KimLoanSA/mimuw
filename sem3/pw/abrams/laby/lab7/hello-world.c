#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

int main () {
  pid_t pid = getpid();
  printf("Hello from %d\n", pid);
}