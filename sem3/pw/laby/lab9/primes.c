#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <pthread.h>
#include "err.h"

#define NUM 10
#define NAP 1
#define LIMIT 1000

void *worker(void *data) {
  pid_t pid = getpid();
  pthread_t tid = pthread_self();
  int id;
  int prime = 1;
  long number, divisor;
  int *prime_count = malloc(sizeof(int));
  *prime_count = 0;

  id = *((int *) data);
  free(data);

  printf("I am thread %d, my pid=%d and my tid=%lu\n", id, pid, tid);
  for (number = 2L + id; number < LIMIT; number += NUM) {
    prime = 1;
    for (divisor = 2L; divisor <= number / 2; divisor++) {
      if (number % divisor == 0) {
        prime = 0;
        break;
      }
    }
    if (prime == 1) {
      printf("Thread %d: %ld is prime\n", id, number);
      (*prime_count)++;
    }
  }
  return prime_count;
}

int main(int argc, char *argv[]) {
  pthread_t th[NUM];
  pthread_attr_t attr;
  int i, err;
  int *res;
  pid_t pid = getpid();
  int *worker_arg;

  printf("Process %d is creating threads\n", pid);

  if ((err = pthread_attr_init(&attr)) != 0) syserr(err, "attr_init");

  if ((err = pthread_attr_setdetachstate(&attr,
    argc > 1 ? PTHREAD_CREATE_DETACHED : PTHREAD_CREATE_JOINABLE)) != 0)
    syserr(err, "setdetach");

  for (i = 0; i < NUM; i++) {
    worker_arg = malloc(sizeof(int));
    *worker_arg = i;
    if ((err = pthread_create(&th[i], &attr, worker, worker_arg)) != 0)
      syserr(err, "create");
  }

  if (argc > 1) {
    sleep(NAP);
    printf("Main thread finished\n");
  } else {
    printf("Main thread is waiting for workers\n");
    for (i = 0; i < NUM; i++) {
      if ((err = pthread_join(th[i], (void **) &res)) != 0) syserr(err, "join");
      printf("thread %d prime count %d\n", i, *res);
      free(res);
    }
  }

  if ((err = pthread_attr_destroy(&attr)) != 0) syserr(err, "attr_destroy");
  return 0;
}

