#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>

#include "err.h"

/*Stałe opisujące długość wywołań sleep()*/
#define PARENT_SLEEP  2
#define CHILD_SLEEP   4
#define EXTRA_NAP     4

//extern int errno;

void catch_parent (int sig) { printf("Rodzic: Dostałem sygnał >>%s<<\n", strsignal(sig)); }
void catch_child (int sig) { printf("Dziecko: Dostałem sygnał >>%s<<\n", strsignal(sig)); }

/**********************************************************************/
/**********************************************************************/

void parent(pid_t child_pid){
  
  struct sigaction action;
  sigset_t block_mask;

  sigemptyset (&block_mask);
  action.sa_handler = catch_parent;
  action.sa_mask = block_mask;
  action.sa_flags = 0;
  
  if (sigaction (SIGUSR1, &action, 0) == -1)        /*Nowa obługa SIGUSR1*/
    syserr("sigaction");
  if (sigaction (SIGINT, &action, 0) == -1)              /*Nowa obługa SIGINT*/
    syserr("sigaction");
  
  sleep(PARENT_SLEEP);                                         /*czas na przygotowanie dziecka*/
  
  printf("Rodzic: wysyłam sygnał >>%s<<\n", strsignal(SIGINT));
  
  if(kill(child_pid, SIGINT) == -1)
    syserr("kill");
  
  printf("Rodzic: wysłałem sygnał\n");
  
  if(pause() == -1){                                /*Czekam na sygnał*/
    if(errno != EINTR)
      syserr("Rodzic: pause 1");
    else
      printf("Rodzic: obudzono mnie 1\n");
  }
  
  printf("Rodzic: wysyłam sygnał >>%s<<\n", strsignal(SIGINT));
  if(kill(child_pid, SIGINT) == -1)                 /*Dziecko powinno blokować ten sygnał*/
    syserr("Rodzic: kill");
  printf("Rodzic: wysłałem sygnał\n");
  
  
  //sleep(EXTRA_NAP);                                        /*Co się stanie jak odkomentuje?*/
  
  printf("Rodzic: wysyłam ponownie sygnał >>%s<<\n", strsignal(SIGINT));
  if(kill(child_pid, SIGINT) == -1)                 /*Dziecka może nie być*/
    syserr("Rodzic: kill fail");
  else  
    printf("Rodzic: co się stanie z tym sygnałem?\n");
  
  
  wait(0);
  
  printf("Rodzic: wysyłam sygnał >>%s<<\n", strsignal(SIGINT));
  if(kill(child_pid, SIGINT) == -1){                /*Dziecka nie ma, wysyłam sygnał*/
    printf("Rodzic: wszystko ok, brak dziecka: >>%s<<\n", strerror(errno));
  }else  
    syserr("Rodzic: kill fail\n");
  printf("Rodzic: wysłałem? sygnał\n");
}

/**********************************************************************/
/**********************************************************************/

void child(pid_t parent_pid){
  
  struct sigaction action;
  sigset_t block_mask, pending_mask;

  sigemptyset (&block_mask);
  action.sa_handler = catch_child;
  action.sa_mask = block_mask;
  action.sa_flags = 0;
  
  if (sigaction (SIGINT, &action, 0) == -1)              /*Nowa obługa SIGINT*/
    syserr("sigaction");
    
  if(pause() == -1){                                     /*Czekam na sygnał*/
    if(errno != EINTR)
      syserr("pause 1");
    else
      printf("Dziecko: obudzono mnie 1\n");
  }
  
  sigaddset(&block_mask, SIGINT);                        /*Mają nie przeszkadzać*/
  sigaddset(&block_mask, SIGUSR1);
  if (sigprocmask(SIG_BLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask block");
  
  
  printf("Dziecko: wysyłam sygnał >>%s<<\n", strsignal(SIGUSR1));
  if(kill(parent_pid, SIGUSR1) == -1)
    syserr("kill");
  printf("Dziecko: wysłałem sygnał\n");
  printf("Dziecko: wysyłam sygnał >>%s<<\n", strsignal(SIGINT));
  if(kill(parent_pid, SIGINT) == -1)
    syserr("kill");
  printf("Dziecko: wysłałem sygnał\n");
  
  sleep(CHILD_SLEEP);                                               /*Brak blokowania*/
  
  if (sigpending(&pending_mask) == -1)
    syserr("sigpendind");
  if (sigismember(&pending_mask, SIGINT))
    printf("Dziecko: Sygnał czeka na obsługę\n");
  else
    printf("Dziecko: Rodzic nie wysłał mi sygnału\n");
  if (sigprocmask(SIG_UNBLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask unblock");
  
  //sleep(EXTRA_NAP2);
}

/**********************************************************************/
/**********************************************************************/

int main ()
{
  pid_t pid_child, parent_pid;

  parent_pid = getpid();

  switch (pid_child = fork()){
    case -1:
      syserr("Error in fork\n");
    case 0:
      child(parent_pid);
      break;
    default:
      parent(pid_child);
  }
  
    printf("%s: Kończę\n", (getpid() == parent_pid)? "Rodzic":"Dziecko");
  
  return 0;  
}
/**********************************************************************/
