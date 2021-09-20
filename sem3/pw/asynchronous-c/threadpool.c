#include "threadpool.h"

#define ERROR_CODE -1
#define NO_ERROR_CODE 0

//==============================================================================
// queue

struct _queue_element {
  _queue_element_t *next_element;

  runnable_t runnable;
};

struct _queue {
  _queue_element_t *head;
  _queue_element_t *tail;

  size_t size;
};

_queue_t *_create_queue() {
  _queue_t *new_queue = malloc(sizeof(_queue_t));
  if (new_queue == NULL) {
    return NULL;
  }

  new_queue->head = NULL;
  new_queue->tail = NULL;
  new_queue->size = 0;

  return new_queue;
}

int _add_to_queue(_queue_t *queue, runnable_t runnable) {
  _queue_element_t *new_queue_element = malloc(sizeof(_queue_element_t));
  if (new_queue_element == NULL) {
    return ERROR_CODE;
  }

  new_queue_element->runnable = runnable;
  new_queue_element->next_element = NULL;

  if (queue->size == 0) {
    queue->head = new_queue_element;
  } else {
    queue->tail->next_element = new_queue_element;
  }

  queue->tail = new_queue_element;
  queue->size++;

  return 0;
}

runnable_t _get_first_from_queue(_queue_t *queue) {
  runnable_t runnable = queue->head->runnable;
  _queue_element_t *first_element = queue->head;

  queue->head = queue->head->next_element;
  free(first_element);

  if (queue->size == 1) {
    queue->tail = NULL;
  }
  queue->size--;

  return runnable;
}

size_t _queue_size(_queue_t *queue) {
  return queue->size;
}

void _destroy_queue(_queue_t *queue) {
  while(_queue_size(queue) > 0) {
    _get_first_from_queue(queue);
  }
}

//==============================================================================
// implementation

int _join_threads(thread_pool_t *pool) {
  for (size_t i = 0; i < pool->size; i++) {
    if (pthread_join(*(pool->threads[i]), 0) != 0) {
      exit(1);
    }
  }

  return 0;
}

void _delete_thread_pool(thread_pool_t *pool) {
  if (pool == NULL) {
    return;
  }

  if (pool->mutex != NULL
    && pthread_mutex_destroy(pool->mutex) != 0) {
    exit(1);
  }
  free(pool->mutex);

  if (pool->init_mutex != NULL
    && pthread_mutex_destroy(pool->init_mutex) != 0) {
    exit(1);
  }
  free(pool->init_mutex);

  if (pool->init_main_thread_mutex != NULL
    && sem_destroy(pool->init_main_thread_mutex) != 0) {
    exit(1);
  }
  free(pool->init_main_thread_mutex);

  if (pool->thread_notification != NULL
    && pthread_cond_destroy(pool->thread_notification) != 0) {
    exit(1);
  }
  free(pool->thread_notification);

  if (pool->threads_attr != NULL
    && pthread_attr_destroy(pool->threads_attr) != 0) {
    exit(1);
  }
  free(pool->threads_attr);

  _destroy_queue(pool->thread_queue);
  free(pool->thread_queue);

  for (size_t i = 0; i < pool->size; i++) {
    free(pool->threads[i]);
  }
  free(pool->threads);
}

void _thread_pool_worker_init(thread_pool_t *pool) {
  if (pthread_mutex_lock(pool->init_mutex) != 0) {
    exit(1);
  }

  pool->init_threads_counter--;

  if (pool->init_threads_counter == 0
    && sem_post(pool->init_main_thread_mutex) != 0) {
    exit(1);
  }

  if (pthread_mutex_unlock(pool->init_mutex) != 0) {
    exit(1);
  }
}

void *_thread_pool_worker(void *thread_pool) {
  thread_pool_t *pool = (thread_pool_t *) thread_pool;

  _thread_pool_worker_init(pool);

  while (1) {
    if (pthread_mutex_lock(pool->mutex) != 0) {
      exit(1);
    }

    while (pool->is_interrupted == 0 && (_queue_size(pool->thread_queue) == 0)) {
      if (pthread_cond_wait(pool->thread_notification, pool->mutex) != 0) {
        exit(1);
      }
    }

    if (pool->is_interrupted == 1) {
      if (pthread_mutex_unlock(pool->mutex) != 0) {
        exit(1);
      }

      return NULL;
    }

    runnable_t runnable = _get_first_from_queue(pool->thread_queue);

    if (pthread_mutex_unlock(pool->mutex) != 0) {
      exit(1);
    }

    (*(runnable.function))(runnable.arg, runnable.argsz);
  }
}

int _malloc_thread_pool(thread_pool_t *pool, int num_threads) {
  pool->mutex = malloc(sizeof(pthread_mutex_t));
  if (pool->mutex == NULL
    || pthread_mutex_init(pool->mutex, 0) != 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  pool->init_mutex = malloc(sizeof(pthread_mutex_t));
  if (pool->init_mutex == NULL
    || pthread_mutex_init(pool->init_mutex, 0) != 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  pool->init_main_thread_mutex = malloc(sizeof(sem_t));
  if (pool->init_main_thread_mutex == NULL
    || sem_init(pool->init_main_thread_mutex, 0, 0) != 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  pool->thread_notification = malloc(sizeof(pthread_cond_t));
  if (pool->thread_notification == NULL
    || pthread_cond_init(pool->thread_notification, 0) != 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  pool->thread_queue = _create_queue();
  if (pool->thread_queue == NULL) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  pool->threads_attr = malloc(sizeof(pthread_attr_t));
  if (pool->threads_attr == NULL
    || pthread_attr_init(pool->threads_attr)!= 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  if (pthread_attr_setdetachstate(pool->threads_attr, PTHREAD_CREATE_JOINABLE) != 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  pool->threads = malloc(sizeof(pthread_t*) * num_threads);
  if (pool->threads == NULL) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  return NO_ERROR_CODE;
}

int thread_pool_init(thread_pool_t *pool, size_t num_threads) {
  if (_malloc_thread_pool(pool, num_threads) != 0) {
    return ERROR_CODE;
  }

  if (pthread_mutex_lock(pool->mutex) != 0) {
    exit(1);
  }

  pool->size = num_threads;
  pool->is_interrupted = 0;
  pool->init_threads_counter = num_threads;

  for (size_t i = 0; i < num_threads; i++) {
    pool->threads[i] = malloc(sizeof(pthread_t));

    if (pool->threads[i] ==  NULL
      || pthread_create(pool->threads[i], pool->threads_attr, _thread_pool_worker, pool) != 0) {
      _delete_thread_pool(pool);
      return ERROR_CODE;
    }
  }

  if (sem_wait(pool->init_main_thread_mutex) != 0) {
    exit(1);
  }

  if (pthread_mutex_unlock(pool->mutex) != 0) {
    exit(1);
  }

  return NO_ERROR_CODE;
}

void thread_pool_destroy(thread_pool_t *pool) {
  if (pthread_mutex_lock(pool->mutex) != 0) {
    exit(1);
  }

  if (pthread_cond_broadcast(pool->thread_notification) != 0) {
    exit(1);
  }

  pool->is_interrupted = 1;

  if (pthread_mutex_unlock(pool->mutex) != 0) {
    exit(1);
  }

  if (_join_threads(pool) != 0) {
    exit(1);
  }

  _delete_thread_pool(pool);
}

int defer(thread_pool_t *pool, runnable_t runnable) {
  if (pthread_mutex_lock(pool->mutex) != 0) {
    exit(1);
  }

  if (pool->is_interrupted) {
    return ERROR_CODE;;
  }

  if (_add_to_queue(pool->thread_queue, runnable) != 0) {
    _delete_thread_pool(pool);
    return ERROR_CODE;
  }

  if (pthread_cond_signal(pool->thread_notification) != 0) {
    exit(1);
  }

  if (pthread_mutex_unlock(pool->mutex) != 0) {
    exit(1);
  }

  return NO_ERROR_CODE;
}
