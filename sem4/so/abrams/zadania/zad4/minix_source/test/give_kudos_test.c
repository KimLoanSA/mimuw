#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/types.h>
#include <fcntl.h>

int first_child_pid;

int main() {
  printf("startowy pid: %d\n", getpid());
  printf("liczba kudosow startowego: 0\n");

  switch (first_child_pid = fork()) {
    case 0:
      first_child_pid = getpid();
      printf("pid dziecka: %d\n", first_child_pid);
      usleep(60000);

      return 0;

    default:
      usleep(1000);
      printf("pid dziecka w rodzicu: %d\n", first_child_pid);
      printf("probuje dac kudosy swojemu dziecku: nie powinno sie udac (moj pid: %d)\n", getpid());
      givekudos(first_child_pid);
      printf("nie udalo sie? sprawdz! (moj pid: %d)\n", getpid());

      break;
  }

  switch (fork()) {
    case 0:
      usleep(1000);
      printf("pid dziecka w rodzicu: %d\n", first_child_pid);
      printf("probuje dac kudosy swojemu kuzynowi: powinno sie udac (moj pid: %d)\n", getpid());
      givekudos(first_child_pid);
      printf("udalo sie? sprawdz! (moj pid: %d)\n", getpid());

      break;

    default:
      usleep(1000);

      wait(0);

      break;
  }

  return 0;
}