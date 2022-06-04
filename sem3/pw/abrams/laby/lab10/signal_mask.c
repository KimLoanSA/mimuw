#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>

#include "err.h"


int main(void){
  
  sigset_t block_mask, pending_mask;

  sigemptyset (&block_mask);
  sigaddset(&block_mask, SIGINT);                        /*Blokuję SIGINT*/
  if (sigprocmask(SIG_BLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask block");
  
  printf("Mój pid to %d\n", getpid());
  printf("Przez 5 sekund będę blokował sygnał %s\n", strsignal(SIGINT));
  printf("Wciśnięcie Ctrl + c spowoduje wysłanie tego sygnału.\n");
  
  sleep(5);
  
  printf("Zakończyłem blokowanie.\n");
  
  if (sigpending(&pending_mask) == -1)
    syserr("sigpending");
  if (sigismember(&pending_mask, SIGINT)){
    printf("Wysłano sygnał %s\n", strsignal(SIGINT));
    printf("Teraz go odblokuję i zakończę działanie\n");
  }
  
  if (sigprocmask(SIG_UNBLOCK, &block_mask, 0) == -1)
    syserr("sigprocmask unblock");  
  
  printf("Nie otrzymałem sygnału, wyślij sygnał by zakończyć\n");
  
  pause(); /*pause nie wróci, jeśli domyślna akcja dla sygnału to zakończ*/
  printf("Pause zostało przerwane\n");
  
  return 0;
}

