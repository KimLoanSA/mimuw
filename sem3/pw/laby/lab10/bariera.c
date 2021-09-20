#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include "err.h"


/*Liczba dzieci*/
#define N   10
#define K   5

//SIGUSR1

int signal_count = 0;
int pipe_dsc[2];
extern int errno;

void catch(int sig, siginfo_t *info, void *more) {
  signal_count++;

  if (signal_count == K) {
    printf("Jestem bariera i otrzymalem K sygnalow, wiec zamytam bariere\n\n");

    // Zamykam pipe
    if (close(pipe_dsc[0]) == -1) {
      syserr("Error in close(pipe_dsc[0])\n");
    }
  } else {
    printf("Jestem bariera i otrzymalem sygnal %s\n byl on %d. sygnalem\n\n",
      strsignal(sig), signal_count);
  }
}

int main() {

  pid_t parent_pid, child_pid[N];
  srand((unsigned) time(0));  // losowość
  char buf[7];

  struct sigaction action;
  sigset_t block_mask;

  sigemptyset (&block_mask);
  sigaddset(&block_mask, SIGUSR1);

  action.sa_sigaction = catch;
  action.sa_mask = block_mask;
  action.sa_flags = 0;

  // Moja obsluga
  if (sigaction(SIGUSR1, &action, 0) == -1) {
    syserr("sigaction");
  }

  //otwieram pipe
  if (pipe(pipe_dsc) == -1) {
    syserr("Error in pipe\n");
  }

  parent_pid = getpid();

  for (int i = 0; i < N; ++i) {    //tworzę N dzieci
    // pseudolosowość wymaga losowania w rodzicu
    int sleep_time = rand() % 10 + 1;
    switch (child_pid[i] = fork()) {
      case -1:
        syserr("Error in fork\n");
      case 0:
        /*child*/

        printf("Dziecko %d: Zaczynam\n\n", getpid());

        // Jestem dzieckiem wiec zamykam pisanie
        if (close(pipe_dsc[1]) == -1) {
          syserr("Error in close(pipe_dsc[1])\n");
        }

        sleep(sleep_time);  // wymagana drzemka

        printf("Dziecko %d: Wysylam sygnal do ojca\n", getpid());
        //wysylam do ojca
        if (kill(parent_pid, SIGUSR1) == -1) {
          syserr("kill");
        }

        printf("Dziecko %d: Czekam na pozwolenie od ojca\n", getpid());

        // Zeby czekac to czytam z pipa
        if (read(pipe_dsc[0], buf, 1) == -1) {
          syserr("Error in read\n");;
        }

        printf("Dziecko %d: Kończę\n\n", getpid());

        return 0;
      default:
#ifdef D
        printf("Parent %d\n", i);
#endif
        break;
    }
  }

  /*parent*/
  int i = 0;
  while (i < N) {
    printf("Czekam na dziecko\n");
    if (wait(0) == -1) {
      if (errno != EINTR)
        syserr("wait");
    } else {
      printf("Odebrałem dziecko\n");
      ++i;
    }
  }

  // Zamykam pipe
  if (close(pipe_dsc[0]) == -1) {
    syserr("Error in close(pipe_dsc[0])\n");
  }

  printf("Rodzic: Kończę\n");

  return 0;
}
/**********************************************************************/
