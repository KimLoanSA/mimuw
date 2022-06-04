#include "future.h"

#define ERROR_CODE -1
#define NO_ERROR_CODE 0

typedef void *(*function_t)(void *);

void _delete_future(future_t *future, int first, int second) {

  if (first && pthread_mutex_destroy(&future->mutex) != 0) {
    exit(1);
  }

  if (second && sem_destroy(&future->sem) != 0) {
    exit(1);
  }
}

void _call_handler_if_needed(future_t *future) {
  if (future->is_handler) {
    _post_call_handler_t *handler = &future->post_call_handler;
    (*(handler->handler))(handler->pool, handler->future, future);
  }
}

void _future_thread_pool_worker(void *arg, size_t size __attribute__((unused))) {
  future_t *future = (future_t *)arg;
  callable_t *callable = &(future->callable);

  void *result = (*(callable->function))
    (callable->arg, callable->argsz, &future->result_size);

  if (pthread_mutex_lock(&future->mutex) != 0) {
    exit(1);
  }

  future->is_result = 1;
  future->result = result;

  int to_delete = future->is_handler;

  _call_handler_if_needed(future);

  if (sem_post(&future->sem) != 0) {
    exit(1);
  }

  if (pthread_mutex_unlock(&future->mutex) != 0) {
    exit(1);
  }

  if (to_delete) {
    _delete_future(future, 1, 1);
  }
}

runnable_t _create_runnable_from_future(future_t *future) {
  runnable_t runnable;
  runnable.function = _future_thread_pool_worker;
  runnable.arg = future;

  return runnable;
}

void _post_call_handler(thread_pool_t *pool, future_t *future, future_t *from) {
  future->callable.arg = from->result;
  future->callable.argsz = from->result_size;

  if (defer(pool, _create_runnable_from_future(future)) != 0) {
    exit(1);
  }
}

int _init_future(future_t *future, callable_t callable) {
  if (pthread_mutex_init(&future->mutex, 0) != 0) {
    return ERROR_CODE;
  }

  if (sem_init(&future->sem, 0, 0) != 0) {
    _delete_future(future, 1, 0);
    return ERROR_CODE;
  }

  future->callable = callable;
  future->is_handler = 0;
  future->result_size = 0;
  future->result = NULL;
  future->is_result = 0;

  return NO_ERROR_CODE;
}

int async(thread_pool_t *pool, future_t *future, callable_t callable) {
  if (_init_future(future, callable) != 0) {
    return ERROR_CODE;
  }

  return defer(pool, _create_runnable_from_future(future));
}

_post_call_handler_t _create_handler(thread_pool_t *pool, future_t *future) {
  _post_call_handler_t handler;
  handler.future = future;
  handler.pool = pool;
  handler.handler = _post_call_handler;

  return handler;
}

int _map_if_is_result(thread_pool_t *pool, future_t *future, future_t *from,
  callable_t *callable) {
  (*callable).arg = from->result;
  (*callable).argsz = from->result_size;

  if (pthread_mutex_unlock(&from->mutex) != 0) {
    exit(1);
  }

  _delete_future(from, 1, 1);

  return async(pool, future, (*callable));
}

int _map_if_is_not_result(thread_pool_t *pool, future_t *future, future_t *from) {
  from->is_handler = 1;
  from->post_call_handler = _create_handler(pool, future);

  if (pthread_mutex_unlock(&from->mutex) != 0) {
    exit(1);
  }

  return NO_ERROR_CODE;
}

int map(thread_pool_t *pool, future_t *future, future_t *from,
        void *(*function)(void *, size_t, size_t *)) {
  callable_t callable;
  callable.function = function;

  if (_init_future(future, callable) != 0) {
    return ERROR_CODE;
  }

  if (pthread_mutex_lock(&from->mutex) != 0) {
    exit(1);
  }

  if (from->is_result) {
    return _map_if_is_result(pool, future, from, &callable);
  } else {
    return _map_if_is_not_result(pool, future, from);
  }
}

void *await(future_t *future) {
  if (pthread_mutex_lock(&future->mutex) != 0) {
    exit(1);
  }

  if (future->is_result == 0) {
    if (pthread_mutex_unlock(&future->mutex) != 0) {
      exit(1);
    }

    if (sem_wait(&future->sem) != 0) {
      exit(1);
    }

    if (pthread_mutex_lock(&future->mutex) != 0) {
      exit(1);
    }
  }

  void *result = future->result;

  if (pthread_mutex_unlock(&future->mutex) != 0) {
    exit(1);
  }

  _delete_future(future, 1, 1);

  return result;
}
