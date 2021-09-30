#include <unistd.h>

int main() {

  printf("pid dziadka: %d\n", getppid());

  switch(fork()) {
    case 0:
      printf("pid tatusia: %d\n", getppid());
      changeparent();
    printf("pid tatusia ale juz powinien byc jak dziadka: %d\n", getppid());
  }

  return 0;
}