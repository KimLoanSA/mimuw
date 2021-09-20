#include <stdio.h>

#include "threadpool.h"
#include "future.h"

#define THREAD_POOL_SIZE 3L

typedef struct factorial_info {
  unsigned long long val;
  unsigned long long res;
} factorial_info_t;

static void *factorial_identity(void *arg, size_t argsz __attribute__((unused)),
  size_t *retsz __attribute__((unused))) {
  return arg;
}

static void* factorial_next(void *arg, size_t argsz __attribute__((unused)),
  size_t *retsz __attribute__((unused))) {
  factorial_info_t *info = (factorial_info_t *)arg;
  unsigned long long val = info->val + THREAD_POOL_SIZE;

  factorial_info_t *new_info = malloc(sizeof(factorial_info_t));
  new_info->res = info->res * val;
  new_info->val = val;

  free(arg);

  return new_info;
}

factorial_info_t *create_factorial_info(unsigned long long val) {
  factorial_info_t *info = malloc(sizeof(factorial_info_t));
  info->res = val;
  info->val = val;

  return info;
}

callable_t create_callable(factorial_info_t *info) {
  callable_t callable;
  callable.function = factorial_identity;
  callable.arg = info;

  return callable;
}

unsigned long long get_result(void *arg) {
  factorial_info_t *info = (factorial_info_t *)arg;
  unsigned long long res = info->res;

  free(arg);
  return res;
}

void init_futures(long long n, thread_pool_t *thpool, future_t **future) {
  if (future == NULL) {
    exit(1);
  }

  for (long long i = 0; i <= n; i++) {
    future[i] = malloc(sizeof(future_t));
    if (future[i] == NULL) {
      exit(1);
    }
  }

  async(thpool, future[0], create_callable(create_factorial_info(1)));
  for (long long i = 1; i <= THREAD_POOL_SIZE && i <= n; i++) {
    async(thpool, future[i], create_callable(create_factorial_info(i)));
  }
}

void delete_future(long long int n, future_t **future) {
  if (n > 2) {
    get_result(await(future[0]));
  }
  for (long long i = 0; i <= n; i++) {
    free(future[i]);
  }
  free(future);
}


int main() {
  long long n;

  thread_pool_t thpool;
  thread_pool_init(&thpool, THREAD_POOL_SIZE);

  scanf("%lld", &n);

  future_t **future = malloc((n + 1) * sizeof(future_t));
  init_futures(n, &thpool, future);

  for (long long i = THREAD_POOL_SIZE + 1; i <= n; i++) {
      map(&thpool, future[i], future[i - THREAD_POOL_SIZE], factorial_next);
  }

  unsigned long long res = 1;
  long long index = n - THREAD_POOL_SIZE + 1;
  if (index < 0) {
    index = 0;
  }

  for (long long i = index; i <= n; i++ ) {
    res *= get_result(await(future[i]));
  }

  printf("%lld\n", res);

  delete_future(n, future);
  thread_pool_destroy(&thpool);

  return 0;
}