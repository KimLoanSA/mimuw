#include <time.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>        /* For mode constants */
#include <fcntl.h>           /* For O_* constants */

#include "err.h"


#define NAP_TIME 2            //sleep
#define BUFF_SIZE 12          //bufor


void print_table(char *t, int len){
  
  int i;
  
  printf("Proces %d, tablica pod adresem %p:\n", getpid(), t);
  for(i = 0; i < len; ++i)
    printf("|%c", t[i]);
  printf("|\n\n");
  
  return;
}

int main(){
  
  char buff[BUFF_SIZE] = "Ala ma kota";
  char *mapped_mem;
  int fd_memory = -1; /* deskryptor dla pamięci*/
  int flags, prot;
  pid_t pid;
    
  printf("Wielkość strony to %lu\n", sysconf(_SC_PAGE_SIZE));
    
  prot = PROT_READ | PROT_WRITE;
  flags = MAP_SHARED | MAP_ANONYMOUS; // nie ma pliku, fd winno być -1
  //flags = MAP_PRIVATE;
  //zarezerwuj dwie strony
  mapped_mem = (char *) mmap(NULL, BUFF_SIZE, prot, flags,
                  fd_memory, 0);
  
  if(mapped_mem == MAP_FAILED)
    syserr("mmap");

  print_table(mapped_mem, BUFF_SIZE);
  
  sleep(NAP_TIME);
    
  switch(pid = fork()){
    case -1:
      syserr("fork");
    case 0:
      sleep(NAP_TIME);
      print_table(mapped_mem, BUFF_SIZE);
      sleep(2* NAP_TIME);
      print_table(mapped_mem, BUFF_SIZE);
      munmap(mapped_mem, BUFF_SIZE); // i tak zmiknie wraz z procesem
      return 0;
    default:
      break;
  }
  
  printf("Pid rodzica %d, pid dziecka: %d\n", getpid(), pid);
  print_table(mapped_mem, BUFF_SIZE);
  sleep(2* NAP_TIME);
  
  
  strncpy(mapped_mem, buff, BUFF_SIZE);
  strncpy(mapped_mem, buff, BUFF_SIZE);
  printf("Proces %d, Zmieniłem zawartosć pamięci\n", getpid());
  print_table(mapped_mem, BUFF_SIZE);

  
  wait(0);
  return 0;
}
