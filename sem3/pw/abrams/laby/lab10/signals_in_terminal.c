#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>

#include "err.h"

/* Bardzo prosty program, służy do prezentacji
 * przesyłania sygnałów w konsoli 
 * uruchom w terminalu
 * otworz nowa instancje terminala
 * ustal pid p tego programu (wypisany na wyjściu lub za pomocą polecenia: $ ps -a)
 * użyj polecenia $ kill -signo p by wysłać sygnał
 * by wypisć listę sygnałów wywołaj $ kill -l 
 * co się stanie jeśli wykonamy
 * $ kill -SIGSTOP p
 * $ kill -SIGINT  p
 * ?
 * Kiedy SIGINT zostanie odebrany?
 * co się stanie jeśli wykonamy
 * $ kill -SIGSTOP p
 * $ kill -SIGCONT  p
 * ?
 */

int main(void){
  
  int i = 0;
  
  printf("Mój pid to %d\n",getpid());
  
  for(;;++i){
    
    sleep(1);
    printf("%d\n",i);
    
  }
  
  return 0;
}

