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


#define SHM_NAME           "/pw_practice_memory"
#define SEM_NAME           "/pw_practice_sem"
#define NAP_TIME           2
#define BUFF_SIZE          12



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
  sem_t *sem;
    
  printf("Wielkość strony to %lu\n", sysconf(_SC_PAGE_SIZE));
  
  //pamięć współdzielona
  fd_memory = shm_open(SHM_NAME, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  if(fd_memory == -1) syserr("shm_open");
  
  if (ftruncate(fd_memory, BUFF_SIZE) == -1)
    syserr("truncate");
  
 
  prot = PROT_READ | PROT_WRITE;
  flags = MAP_SHARED;
  mapped_mem = (char *) mmap(NULL, BUFF_SIZE, prot, flags,
                  fd_memory, 0);
  
  /*Plik specjalny nam już niepotrzebny*/
  close(fd_memory); /*plik specjalny dalej istnieje*/
  shm_unlink(SHM_NAME); /*usunęliśmy plik specjalny*/ //Co się stanie, jeśli zakomentuję?
  
  if(mapped_mem == MAP_FAILED)
    syserr("mmap");

  print_table(mapped_mem, BUFF_SIZE);
  
  //semafor
  sem = sem_open(SEM_NAME, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR, 0);
  if (sem == SEM_FAILED)
    syserr("parent sem_open");
  if (sem_close(sem))
        syserr("parent sem_close");
  
    
  switch(pid = fork()){
    case -1:
      syserr("fork");
    case 0:
      //częsć wartosci będzie zignorowana
      sem = sem_open(SEM_NAME, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR, 0);
      if (sem == SEM_FAILED)
        syserr("child sem_open");
      if (sem_wait(sem))
        syserr("child sem_wait");
      
      print_table(mapped_mem, BUFF_SIZE);
      munmap(mapped_mem, BUFF_SIZE);
      return 0;
    default:
      printf("Pid rodzica %d, pid dziecka: %d\n", getpid(), pid);
      break;
  }
  //tylko rodzic ma dostep do pamięci
  sleep(NAP_TIME);
  strncpy(mapped_mem, buff, BUFF_SIZE);
  printf("Proces %d, Zmieniłem zawartosć pamięci\n", getpid());
  print_table(mapped_mem, BUFF_SIZE);
  sem = sem_open(SEM_NAME, O_RDWR); // skrócona wersja bez O_CREAT
  if (sem == SEM_FAILED)
    syserr("parent sem_open 2");
  if (sem_post(sem))
    syserr("parent sem_post");
  
  //sprzątanie
  wait(0);
  sem_close(sem);
  sem_unlink(SEM_NAME);
  munmap(mapped_mem, BUFF_SIZE); // w zasadzie niepotrzebne
  
  return 0;
}
