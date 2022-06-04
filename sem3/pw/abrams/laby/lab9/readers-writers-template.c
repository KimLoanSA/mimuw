#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include "err.h"

#define READERS 3
#define WRITERS 2
#define NAP 2
#define BSIZE 32

struct readwrite {
  pthread_mutex_t lock;
  pthread_cond_t readers;
  pthread_cond_t writers;
  int rcount, wcount, rwait, wwait;
  int change;
};

struct readwrite library;
char book[BSIZE];
int working = 1;

/* Initialize a buffer */

void init(struct readwrite *rw) {
  int err;

  if ((err = pthread_mutex_init(&rw->lock, 0)) != 0) {
    syserr(err, "mutex init failed");
  }
  if ((err = pthread_cond_init(&rw->readers, 0)) != 0) {
    syserr(err, "cond init 1 failed");
  }
  if ((err = pthread_cond_init(&rw->writers, 0)) != 0) {
    syserr(err, "cond init 2 failed");
  }

  rw->rcount = 0;
  rw->wcount = 0;
  rw->rwait = 0;
  rw->wwait = 0;
  rw->change = 0;
}

/* Destroy the buffer */

void destroy(struct readwrite *rw) {
  int err;

  if ((err = pthread_cond_destroy(&rw->readers)) != 0) {
    syserr(err, "cond destroy 1 failed");
  }
  if ((err = pthread_cond_destroy(&rw->writers)) != 0) {
    syserr(err, "cond destroy 2 failed");
  }
  if ((err = pthread_mutex_destroy(&rw->lock))) {
    syserr(err, "mutex destroy failed");
  }
}

void *reader(void *data) {
  int err;

  while (working) {
    // biore mutexa
    if ((err = pthread_mutex_lock(&library.lock)) != 0) {
      syserr(err, "lock failed");
    }

    library.rwait++; // bede czekal

    while(library.wwait > 0 && library.wcount > 0 && library.change == 0) { //jesli jest jakis pisarz to czekam
      if ((err = pthread_cond_wait(&library.readers, &library.lock)) != 0) {
        syserr(err, "cond wait failed");
      }
    }
    library.change = 0;

    library.rwait--; // juz nie czekam
    library.rcount++; // bede czytal

    library.change = 1; // bede robil signala wiece change = 1
    if ((err = pthread_cond_signal(&library.readers)) != 0) { //wybudzam innych
      syserr(err, "cond signal failed");
    }

    if ((err = pthread_mutex_unlock(&library.lock)) != 0) { //oddaje mutexa
      syserr(err, "unlock failed");
    }


    printf("reader read: %s\n", book); /* reading */


    // biore mutexa
    if ((err = pthread_mutex_lock(&library.lock)) != 0) {
      syserr(err, "lock failed");
    }

    library.rcount--; // juz nie czytam

    if (library.rcount == 0 && library.wwait > 0) { // jakis pisarz czeka
      library.change = 1; // bede robil signala wiece change = 1

      if ((err = pthread_cond_signal(&library.writers)) != 0) { //wybudzam pisarza
        syserr(err, "cond signal failed");
      }
    }

    if ((err = pthread_mutex_unlock(&library.lock)) != 0) { //oddaje mutexa
      syserr(err, "unlock failed");
    }
  }

  return 0;
}

void *writer(void *data) {
  int l;
  int err;

  while (working) {
    // biore mutexa
    if ((err = pthread_mutex_lock(&library.lock)) != 0) {
      syserr(err, "lock failed");
    }

    library.wwait++; // bede czekal

    while (library.wcount + library.rcount > 0 && library.change == 0) { // w biliotece ktos jest to czekam
      if ((err = pthread_cond_wait(&library.writers, &library.lock)) != 0) {
        syserr(err, "cond wait failed");
      }
    }
    library.change = 0;

    library.wwait--; // juz nie czekam
    library.wcount++; //bede pisal


    if ((err = pthread_mutex_unlock(&library.lock)) != 0) { //oddaje mutexa
      syserr(err, "unlock failed");
    }


    l = rand() % 10;
    snprintf(book, BSIZE, "6 times a number %d %d %d %d %d %d", l, l, l, l, l, l);


    // biore mutexa
    if ((err = pthread_mutex_lock(&library.lock)) != 0) {
      syserr(err, "lock failed");
    }

    library.wcount--; // skonczylem pisac wiec bede wychodzil

    if (library.wwait > 0) {
      library.change = 1;
      if ((err = pthread_cond_signal(&library.writers)) != 0) { //wybudzam innego pisarza
        syserr(err, "cond signal failed");
      }
    } else if (library.rwait > 0) {
      library.change = 1;
      if ((err = pthread_cond_signal(&library.readers)) != 0) { //wybudzam czytelmnikow
        syserr(err, "cond signal failed");
      }
    }

    if ((err = pthread_mutex_unlock(&library.lock)) != 0) { //oddaje mutexa
      syserr(err, "unlock failed");
    }
  }
  return 0;
}


int main() {
  pthread_t th[READERS + WRITERS];
  pthread_attr_t attr;
  int i, err;
  void *retval;

  srand((unsigned) time(0));

  init(&library);
  if ((err = pthread_attr_init(&attr)) != 0)
    syserr(err, "attr_init failed");
  if ((err = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE)) != 0)
    syserr(err, "attr_setdetachstate failed");

  for (i = 0; i < READERS + WRITERS; i++) {
    if (i < READERS) {
      if ((err = pthread_create(&th[i], &attr, reader, 0)) != 0)
        syserr(err, "create failed");
    } else if ((err = pthread_create(&th[i], &attr, writer, 0)) != 0)
      syserr(err, "create failed");
  }

  sleep(NAP);
  working = 0;

  for (i = 0; i < READERS + WRITERS; i++) {
    if ((err = pthread_join(th[i], &retval)) != 0)
      syserr(err, "join failed");
  }

  if ((err = pthread_attr_destroy(&attr)) != 0)
    syserr(err, "cond destroy failed");
  destroy(&library);
  return 0;
}
