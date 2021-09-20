#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>

#include "err.h"

/*Liczba dzieci*/
#define N   5
#define NAP 5
#define SIGRTMIN (SIGUSR1 -  1)
//extern int errno;


void catch (int sig, siginfo_t *info, void *more) { 
  printf("Rodzic: Dostałem sygnał >>%s<< od %d\n", strsignal(sig), info->si_pid);
}

int main ()
{
  pid_t pid_child, parent_pid;
  int i;

  parent_pid = getpid();

  struct sigaction action;
  sigset_t block_mask;

  sigemptyset (&block_mask);
  action.sa_sigaction = catch;
  action.sa_mask = block_mask;
  //action.sa_flags = SA_SIGINFO | SA_NODEFER;
  action.sa_flags = SA_SIGINFO;
  
  if (sigaction (SIGRTMIN+1, &action, 0) == -1)     /*Nowa obługa*/
    syserr("sigaction");


  for(i=0; i< N; ++i)
    switch (pid_child = fork()){
      
      case -1:
        syserr("Error in fork\n");
      case 0:
        printf("Dziecko %d: Wysyłam sygnał\n", getpid());
        if(kill(parent_pid, SIGRTMIN + 1) == -1)
          syserr("kill");
        printf("Dziecko %d: Kończę\n\n", getpid());
        return 0;
      default:
        break;
    }
  
  
  sigaddset(&block_mask, SIGRTMIN+1);
  if (sigprocmask(SIG_BLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask block");
  
  sleep(NAP);
  
  if (sigprocmask(SIG_UNBLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask unblock");
  
  for(i=0;i<N;++i){
    wait(0);
  }
  printf("Rodzic: Kończę\n");
  
  return 0;  
}
/**********************************************************************/
