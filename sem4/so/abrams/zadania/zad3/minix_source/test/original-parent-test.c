#include <unistd.h>

int main() {

    printf("niestety test uzywa `changeparent()`\n");
    printf("pid dziadka: %d %d\n", getppid(), getpid());
    pid_t my_pid = getpid();
    printf("pid najstarszego, powinien byc taki jak dziadka: %d\n",
      getoppid(my_pid));


    switch (fork()) {
      case 0:
        printf("pid pid tatusia: %d %d\n", getppid(), getpid());
        changeparent();
        printf("pid najstarszego, powinien byc taki jak dziadka: %d\n",
          getoppid(my_pid));
    }

  return 0;
}