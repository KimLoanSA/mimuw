#include <time.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <semaphore.h>
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
  void *mapped_mem_all;
  char *mapped_mem;
  int fd_memory = -1; /* deskryptor dla pamięci*/
  int flags, prot;
  sem_t *sem;
  pid_t pid;
    
  printf("Wielkość strony to %lu\n", sysconf(_SC_PAGE_SIZE));
    
  prot = PROT_READ | PROT_WRITE;
  flags = MAP_SHARED | MAP_ANONYMOUS; // nie ma pliku, fd winno być -1
  // zarezerwuj miejsce na semafor i bufor
  mapped_mem_all = mmap(NULL, sizeof(sem_t) + BUFF_SIZE, prot, flags,
                  fd_memory, 0);
  
  if(mapped_mem_all == MAP_FAILED)
    syserr("mmap");
  
  //podział pamięci na semafor i bufor
  mapped_mem = (char *) (mapped_mem_all + sizeof(sem_t));
  sem = (sem_t *) mapped_mem_all;

  print_table(mapped_mem, BUFF_SIZE);
  
  if(sem_init(sem, 1, 0))
    syserr("sem_init");
  
    
  switch(pid = fork()){
    case -1:
      syserr("fork");
    case 0:
      if(sem_wait(sem))
        syserr("sem_wait");
      print_table(mapped_mem, BUFF_SIZE);
      munmap(mapped_mem, BUFF_SIZE); // i tak zmiknie wraz z procesem
      return 0;
    default:
      printf("Pid rodzica %d, pid dziecka: %d\n", getpid(), pid);
      break;
  }
  
  //wymuszenie zasnięcia na semaforze 
  sleep(NAP_TIME);
  
  strncpy(mapped_mem, buff, BUFF_SIZE);
  printf("Proces %d, Zmieniłem zawartosć pamięci\n", getpid());
  print_table(mapped_mem, BUFF_SIZE);
  if(sem_post(sem))
    syserr("sem_post");
  
  //sprzątanie
  wait(0);
  sem_destroy(sem);
  munmap(mapped_mem_all,  sizeof(sem_t) + BUFF_SIZE); // i tak zniknie, kiedy proces zginie

  return 0;
}
