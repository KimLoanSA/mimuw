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


#define SHM_NAME           "/pw_practice_memory"
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
    
  printf("Wielkość strony to %lu\n", sysconf(_SC_PAGE_SIZE));
    
  fd_memory = shm_open(SHM_NAME, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  if(fd_memory == -1) syserr("shm_open");
  
  if (ftruncate(fd_memory, BUFF_SIZE) == -1)
    syserr("truncate");
  
  //flagi dla mmap; wspólne dla dziecka i rodzica
  prot = PROT_READ | PROT_WRITE;
  flags = MAP_SHARED;
  //flags = MAP_PRIVATE;
  
  sleep(NAP_TIME);
    
  switch(pid = fork()){
    case -1:
      syserr("fork");
    case 0:
      //dziecko
      fd_memory = shm_open(SHM_NAME, O_RDWR,S_IRUSR | S_IWUSR);
      if(fd_memory == -1) syserr("child shm_open");
      mapped_mem = (char *) mmap(NULL, BUFF_SIZE, prot, flags,
                  fd_memory, 0);
      if(mapped_mem == MAP_FAILED){
        syserr("mmap");
      }
      print_table(mapped_mem, BUFF_SIZE);
      sleep(NAP_TIME);
      print_table(mapped_mem, BUFF_SIZE);
      sleep(2* NAP_TIME);
      print_table(mapped_mem, BUFF_SIZE);
      munmap(mapped_mem, BUFF_SIZE);
      close(fd_memory); /*plik specjalny dalej istnieje*/
      /*Plik specjalny nam już niepotrzebny*/
      shm_unlink(SHM_NAME); /*usunęliśmy plik specjalny*/ //Co się stanie, jeśli zakomentuję?
      return 0;
    default:
      break;
  }
  //rodzic
  
  //mapuję pamięć
  fd_memory = shm_open(SHM_NAME, O_RDWR, S_IRUSR | S_IWUSR);
      if(fd_memory == -1) syserr("parent shm_open");
  mapped_mem = (char *) mmap(NULL, BUFF_SIZE, prot, flags,
                  fd_memory, 0);
  if(mapped_mem == MAP_FAILED){
        syserr("mmap");
  }                
  close(fd_memory); /*plik specjalny dalej istnieje*/
  
  printf("Pid rodzica %d, pid dziecka: %d\n", getpid(), pid);
  print_table(mapped_mem, BUFF_SIZE);
  sleep(2* NAP_TIME);
  
  
  strncpy(mapped_mem, buff, BUFF_SIZE);
  printf("Proces %d, Zmieniłem zawartosć pamięci\n", getpid());
  print_table(mapped_mem, BUFF_SIZE);
  
  wait(0);
  munmap(mapped_mem, BUFF_SIZE); // w zasadzie niepotrzebne
  
  return 0;
}
