#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>

#include "err.h"

//#define SIGRTMIN SIGUSR1
/*Stałe opisujące długość wywołań sleep()*/
#define EXTRA_NAP     5
#define SIGTOBLOCK    (SIGUSR1)
#define SIGTOBLOCK2   (SIGUSR2)
//extern int errno;

void catch (int sig) { printf("Dostałem sygnał >>%s<<\n", strsignal(sig)); }


/**********************************************************************/
/**********************************************************************/


int main(){
  
  struct sigaction action;
  sigset_t block_mask,block_mask2;

  sigemptyset (&block_mask);
  sigaddset(&block_mask, SIGTOBLOCK);                        /*Mają nie przeszkadzać*/
  sigaddset(&block_mask, SIGTOBLOCK2);
  sigaddset(&block_mask, SIGQUIT);
  
  sigemptyset (&block_mask2);
  
  action.sa_handler = catch;
  action.sa_mask = block_mask;
  action.sa_flags = 0;
  

  if (sigaction (SIGTOBLOCK, &action, 0) == -1)              /*Nowa obługa SIGRTMIN+1*/
    syserr("sigaction");
  if (sigaction (SIGTOBLOCK2, &action, 0) == -1)             /*Nowa obługa SIGRTMIN+1*/
    syserr("sigaction");
  if (sigaction (SIGQUIT, &action, 0) == -1)                 /*Nowa obługa SIGQUIT*/
    syserr("sigaction");  
  
  
  if (sigprocmask(SIG_BLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask block");
    
  
  printf("Moj pid to %d, blokuję sygnały (%d:%s), (%d:%s)", getpid(), SIGTOBLOCK, strsignal(SIGTOBLOCK) , SIGQUIT, strsignal(SIGQUIT));
  printf(" i (%d:%s)\n",SIGTOBLOCK2, strsignal(SIGTOBLOCK2));
  
  printf("Wysyłam do siebie po dwa powyższe sygnały.\n");
  
  if(kill(getpid(), SIGTOBLOCK) == -1)
    syserr("kill");
  if(kill(getpid(), SIGTOBLOCK2) == -1)
    syserr("kill");
  if(kill(getpid(), SIGQUIT) == -1)
    syserr("kill");
  if(kill(getpid(), SIGQUIT) == -1)
    syserr("kill");
  if(kill(getpid(), SIGTOBLOCK) == -1)
    syserr("kill");
  
  
  printf("Usypiam na %d sekund\n", EXTRA_NAP);
  sleep(EXTRA_NAP);
  
  printf("Odblokowuję sygnały\n");
/*
  sigaddset(&block_mask2, SIGTOBLOCK);
  if (sigprocmask(SIG_UNBLOCK, &block_mask2, 0) == -1)
    syserr("sigprocmask unblock");
*/  
  if (sigprocmask(SIG_UNBLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask unblock");
  
  
  return 0;
}
