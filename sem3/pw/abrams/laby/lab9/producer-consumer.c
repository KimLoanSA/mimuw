/* The classic producer-consumer example.
   Illustrates mutexes and conditions.
   All integers between 0 and 9999 should be printed exactly twice,
   once to the right of the arrow and once to the left. */

#include <stdio.h>
#include <pthread.h>
#include "err.h"

#define BUFFER_SIZE 16
 
/* Circular buffer of integers. */ 

struct prodcons {
  int buffer[BUFFER_SIZE];      /* the actual data */
  pthread_mutex_t lock;         /* mutex ensuring exclusive access to buffer */
  int readpos, writepos;        /* positions for reading and writing */
  pthread_cond_t notempty;      /* signaled when buffer is not empty */
  pthread_cond_t notfull;       /* signaled when buffer is not full */
};

/* Initialize a buffer */

void init(struct prodcons *b) {
  int err;
  
  if ((err = pthread_mutex_init(&b->lock, 0)) != 0)
    syserr (err, "mutex init failed");
  if ((err = pthread_cond_init(&b->notempty, 0)) != 0)
    syserr (err, "cond init 1 failed");
  if ((err = pthread_cond_init(&b->notfull, 0)) != 0)
    syserr (err, "cond init 2 failed");
  b->readpos = 0;
  b->writepos = 0;
}

/* Destroy the buffer */

void destroy(struct prodcons *b) {
  int err;

  if ((err = pthread_cond_destroy (&b->notempty)) != 0)
    syserr (err, "cond destroy 1 failed");
  if ((err = pthread_cond_destroy (&b->notfull)) != 0)
    syserr (err, "cond destroy 2 failed");
  if ((err = pthread_mutex_destroy (&b->lock)) != 0)
    syserr (err, "mutex destroy failed");
}

/* Store an integer in the buffer */

void put(struct prodcons *b, int data) {
  int err;
  if ((err = pthread_mutex_lock(&b->lock)) != 0)
    syserr (err, "lock failed");

  /* Wait until buffer is not full */
  while ((b->writepos + 1) % BUFFER_SIZE == b->readpos)
    if ((err = pthread_cond_wait(&b->notfull, &b->lock)) != 0)
      syserr (err, "cond wait failed");
  /* pthread_cond_wait reacquired b->lock before returning */
  
  /* Write the data and advance write pointer */
  b->buffer[b->writepos] = data;
  b->writepos++;
  if (b->writepos >= BUFFER_SIZE) b->writepos = 0;

  /* Signal that the buffer is now not empty */
  if ((err = pthread_cond_signal(&b->notempty)) != 0)
    syserr (err, "cond signal failed");

  if ((err = pthread_mutex_unlock(&b->lock)) != 0)
    syserr (err, "unlock failed");
}

/* Read and remove an integer from the buffer */

int get(struct prodcons *b) {
  int data;
  int err;
  
  if ((err = pthread_mutex_lock(&b->lock)) != 0)
    syserr (err, "lock failed");

  /* Wait until buffer is not empty */
  while (b->writepos == b->readpos) {
    if ((err = pthread_cond_wait(&b->notempty, &b->lock)) != 0)
      syserr (err, "cond wait failed");
  }
  /* Read the data and advance read pointer */
  data = b->buffer[b->readpos];
  b->readpos++;
  if (b->readpos >= BUFFER_SIZE) b->readpos = 0;

  /* Signal that the buffer is now not full */
  if ((err = pthread_cond_signal(&b->notfull)) != 0)
    syserr (err, "cond signal failed");

  if ((err = pthread_mutex_unlock(&b->lock)) != 0)
    syserr (err, "unlock failed");
  return data;
}

/* A test program: one thread inserts integers from 1 to 10000,
   the other reads them and prints them. */

#define OVER (-1)

struct prodcons buffer;

void *producer(void *data) {
  int n;
  for (n = 0; n < 10000; n++) {
    printf("%d --->\n", n);
    put(&buffer, n);
  }
  put(&buffer, OVER);
  return 0;
}

void *consumer(void *data) {
  while (1) {
    int d = get(&buffer);
    if (d == OVER) break;
    printf("---> %d\n", d);
  }
  return 0;
}

int main() {
  pthread_t th_a, th_b;
  pthread_attr_t attr;
  int err;
  void *retval;
  
  init(&buffer);
  if ((err = pthread_attr_init (&attr)) != 0)
    syserr (err, "attr_init failed");
  if ((err = pthread_attr_setdetachstate (&attr,PTHREAD_CREATE_JOINABLE)) != 0)
    syserr (err, "attr_setdetachstate failed");

  /* Create the threads */
  if ((err = pthread_create(&th_a, &attr, producer, 0)) != 0)
    syserr (err, "create 1 failed");
  if ((err = pthread_create(&th_b, &attr, consumer, 0)) != 0)
    syserr (err, "create 2 failed");  

  /* Wait until producer and consumer finish. */
  if ((err = pthread_join(th_a, &retval)) != 0)
    syserr (err, "join 1 failed");
  if ((err = pthread_join(th_b, &retval)) != 0)
    syserr (err, "join 2 failed");

  if ((err = pthread_attr_destroy (&attr)) != 0)
    syserr (err, "cond destroy failed");
  destroy(&buffer);
  return 0;
}


