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

//some consts
#define MAX_REINDEERS 7
#define MAX_ELFS 6
#define MAX_GIFTS 5

#define NO_REINDEERS 3
#define NO_ELFS 4
#define NO_GIFTS 5

#define BUFFSIZE 3
#define LOOPS 5

/**************************************************************************************************/
//storage compartment
struct storage{
  sem_t mutex;
  sem_t buffer_read;
  sem_t buffer_write;

  int buffer[BUFFSIZE];
  int read_index;
  int write_index;
  int elfs_counter;
};
/**************************************************************************************************/
//popular names
char *elfs_names[MAX_ELFS] = {"Mirek","Zuzia","Gienia", "Macius", "Ela", "Stasia"};
char *reindeers_names[MAX_REINDEERS] = {"Janek", "Zosia", "Franek", "Jozek", "Asia", "Olek", "Ruda"};
char *gifts[MAX_GIFTS] = {"lalka", "klocki", "ciuchcia", "rozga", "rower"};

/**************************************************************************************************/
//foymaker
int produce(){
  sleep(rand() % 3);
  return rand()%NO_GIFTS;
}

//sent to santa
void deliver(int i){
  sleep(rand() % 3);
}

void nap(int i){
  sleep(i);
}
/**************************************************************************************************/
//buffer

void add_to_buffer(struct storage *buffer, int gift, int is_last) {
  if (sem_wait(&(buffer->buffer_write))) {
    syserr("buffer write wait");
  }

  if (sem_wait(&(buffer->mutex))) {
    syserr("buffer mutex wait");
  }

  buffer->buffer[buffer->write_index] = gift;
  buffer->elfs_counter += is_last;
  buffer->write_index = (buffer->write_index + 1) % BUFFSIZE;

  if (sem_post(&(buffer->mutex))) {
    syserr("buffer mutex post");
  }

  if (sem_post(&(buffer->buffer_read))) {
    syserr("buffer read post");
  }
}

int get_from_buffer(struct storage *buffer) {

  if (sem_wait(&(buffer->buffer_read))) {
    syserr("buffer read wait");
  }

  if (sem_wait(&(buffer->mutex))) {
    syserr("buffer mutex wait");
  }

  int end = 0;
  int gift = buffer->buffer[buffer->read_index];

  if ((buffer->elfs_counter == NO_ELFS) && (buffer->read_index + 1 == buffer->write_index)) { //jestem ostatnim reniferem
    end = 2;
  } 

  if ((buffer->elfs_counter == NO_ELFS) && (buffer->read_index == buffer->write_index)) { //nie ma juz czego transportowac
    end = 1;
  }

  if (end != 1) {
    buffer->read_index = (buffer->read_index + 1) % BUFFSIZE;
  }

  if (sem_post(&(buffer->mutex))) {
    syserr("buffer mutex post");
  }

  if (end == 0) {
    if (sem_post(&(buffer->buffer_write))) {
      syserr("buffer write post");
    }
  } else {
    if (sem_post(&(buffer->buffer_read))) { // pozwalam innym reniferom sie dowiedziec ze juz nie ma czego transportowac
      syserr("buffer read post");
    }
  }

  return end == 1 ? -1 : gift;
}
/**************************************************************************************************/
//life of an elf
void elf(int id, struct storage* s){

  int i,g;
  printf("Hej! Jestem elfem o imieniu %s, zaczynam!\n", elfs_names[id]);
  for(i = 0; i< LOOPS; ++i){

    g = produce();
    printf("Hej! Jestem elfem o imieniu %s, wyprodukowalem/am prezent: %s\n", elfs_names[id], gifts[g]);

    add_to_buffer(s, g, (i + 1 == LOOPS));

    printf("Hej! Jestem elfem o imieniu %s, wstawilem/am prezent: %s\n", elfs_names[id], gifts[g]);
  }
}

/**************************************************************************************************/
//life of a reindeer
void reindeer(int id, struct storage* s){
  
  int end = 0;
  int g;
  
  printf("Hej! Jestem reniferem o imieniu %s, zaczynam!\n", reindeers_names[id]);
  while(!end){

    g = get_from_buffer(s);

    if (g >= 0) {
      printf("Hej! Jestem reniferem o imieniu %s, odebralem/am prezent: %s\n", reindeers_names[id], gifts[g]);

      deliver(g);
      printf("Hej! Jestem reniferem o imieniu %s dostarczylem/am prezent: %s\n", reindeers_names[id], gifts[g]);
    } else {
      end = 1;
    }
  }
}
/**************************************************************************************************/
/**************************************************************************************************/
int main(){
    
  int i;
  pid_t pid;
  struct storage *mapped_mem;
  
  int seed = time(0);
  srand(seed);

  int prot = PROT_READ | PROT_WRITE;
  int flags = MAP_SHARED | MAP_ANONYMOUS; // nie ma pliku, fd winno być -1
  int fd_memory = -1; /* deskryptor dla pamięci*/

  void *mapped_mem_all = mmap(NULL, sizeof(struct storage), prot, flags, fd_memory, 0);

   if(mapped_mem_all == MAP_FAILED) {
    syserr("mmap");
  }

  mapped_mem = (struct storage *) mapped_mem_all;

  mapped_mem->read_index = 0;
  mapped_mem->write_index = 0;
  mapped_mem->elfs_counter = 0;

  if (sem_init(&mapped_mem->mutex, 1, 1)) {
    syserr("mutex init");
  }

  if (sem_init(&mapped_mem->buffer_read, 1, 0)) {
    syserr("read init");
  }

  if (sem_init(&mapped_mem->buffer_write, 1, BUFFSIZE)) {
    syserr("write buffer init");
  }

  printf("Tworze pracownikow.\nElfy: %d; Renifery: %d\n", NO_ELFS, NO_REINDEERS);
  
  for(i = 0; i < NO_ELFS + NO_REINDEERS; i++){
    
    rand();//some randomness
    switch(pid = fork()){
    case -1:
      syserr("fork");
    case 0:
      srand(rand());
      if (i < NO_ELFS){
        printf("Elf %d!\n", i);
        elf(i, mapped_mem);
        
      }else{
        printf("Renifer %d!\n", i);
        reindeer(i-NO_ELFS,mapped_mem);
      }
      return 0;
    default:
      nap(1);
      printf("Kolejny pracownik!\n");
      break;
    }
  }

  for(i = 0; i< NO_ELFS+NO_REINDEERS; ++i) wait(0);

  sem_destroy(&mapped_mem->mutex);
  sem_destroy(&mapped_mem->buffer_read);
  sem_destroy(&mapped_mem->buffer_write);

  munmap(mapped_mem_all,  sizeof(struct storage)); // i tak zniknie, kiedy proces zginie
  
  return 0;
}
