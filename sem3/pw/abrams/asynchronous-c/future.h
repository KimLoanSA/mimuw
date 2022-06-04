#ifndef FUTURE_H
#define FUTURE_H

#include "threadpool.h"

#include <pthread.h>
#include <semaphore.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct future future_t;

typedef struct _post_call_handler {
  void (*handler)(thread_pool_t *pool, future_t *future, future_t *from);
  thread_pool_t *pool;
  future_t *future;
} _post_call_handler_t;

typedef struct callable {
  void *(*function)(void *, size_t, size_t *);
  void *arg;
  size_t argsz;
} callable_t;

typedef struct future {
  callable_t callable;
  void *result;
  size_t result_size;

  int is_handler;
  _post_call_handler_t post_call_handler;

  pthread_mutex_t mutex;
  int is_result;
  sem_t sem;
} future_t;


int async(thread_pool_t *pool, future_t *future, callable_t callable);

int map(thread_pool_t *pool, future_t *future, future_t *from,
        void *(*function)(void *, size_t, size_t *));

void *await(future_t *future);

#endif