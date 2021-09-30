#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include "err.h"
#include "inc_thread.h"

#define MAX_THREADS 50

static int global_value = 0;

int main (int argc, char *args[]) {
  int i, thread_count;
  thread_data_t td;

  pthread_t tid[MAX_THREADS];

  if (argc != 3)
    fatal("Usage: %s thread_count max_value", args[0]);

  thread_count = atoi(args[1]);
  td.value = &global_value;
  td.count = atoi(args[2]);

  if (thread_count > MAX_THREADS || thread_count < 1)
    fatal("Wrong number of threads %d, should be in range 1...%d.",
      thread_count, MAX_THREADS);

  for (i = 1; i < thread_count; ++i)
    if (pthread_create(&tid[i], NULL, &inc_thread, (void*)&td))
      syserr("Function create_thread failed for thread %d.", i);

  /* Wątek 0 już mamy uruchomiony. */
  inc_thread((void*)&td);

  for (i = 1; i < thread_count; ++i)
    if (pthread_join(tid[i], NULL))
      syserr("Function join_thread failed for thread %d.", i);

  printf("%d\n", global_value);

  return 0;
}