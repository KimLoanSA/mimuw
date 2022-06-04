#ifndef THREADPOOL_H
#define THREADPOOL_H

#include <pthread.h>
#include <semaphore.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct _queue_element _queue_element_t;
typedef struct _queue _queue_t;

typedef struct runnable {
  void (*function)(void *, size_t);
  void *arg;
  size_t argsz;
} runnable_t;

typedef struct thread_pool {
  pthread_t** threads;
  pthread_attr_t *threads_attr;

  pthread_mutex_t *mutex;
  pthread_cond_t *thread_notification;
  _queue_t *thread_queue;

  pthread_mutex_t *init_mutex;
  sem_t *init_main_thread_mutex;
  size_t init_threads_counter;

  size_t size;
  int is_interrupted;

} thread_pool_t;

int thread_pool_init(thread_pool_t *pool, size_t pool_size);

void thread_pool_destroy(thread_pool_t *pool);

int defer(thread_pool_t *pool, runnable_t runnable);

#endif
