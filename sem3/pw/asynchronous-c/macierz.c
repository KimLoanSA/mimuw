#include <stdio.h>
#include <unistd.h>

#include "threadpool.h"

#define THREAD_POOL_SIZE 4

typedef struct matrix_utils {
  pthread_mutex_t *mutex;
  sem_t *sem;

  long long counter;
  long long *result_arr;

} matrix_utils_t;

typedef struct matrix_worker_info {
  matrix_utils_t *matrix;

  long long time;
  long long value;
  long long row;
} matrix_worker_info_t;

void matrix_worker(void *arg, size_t argsz __attribute__((unused))) {
  matrix_worker_info_t *info = (matrix_worker_info_t *) arg;
  matrix_utils_t *matrix = info->matrix;

  usleep(info->time * 1000L);

  if (pthread_mutex_lock(matrix->mutex) != 0) {
    exit(1);
  }

  matrix->result_arr[info->row] += info->value;
  matrix->counter--;

  if (matrix->counter == 0 && sem_post(matrix->sem) != 0) {
    exit(1);
  }

  if (pthread_mutex_unlock(matrix->mutex) != 0) {
    exit(1);
  }
}


void init_matrix_utils(matrix_utils_t *matrix, long long rows,
  long long columns) {
  if (matrix == NULL) {
    exit(1);
  }
  matrix->mutex = malloc(sizeof(pthread_mutex_t));
  if (matrix->mutex == NULL
    || pthread_mutex_init(matrix->mutex, 0) != 0) {
    exit(1);
  }

  matrix->sem = malloc(sizeof(sem_t));
  if (matrix->sem == NULL
    || sem_init(matrix->sem, 0, 0) != 0) {
    exit(1);
  }

  matrix->result_arr = malloc(rows * sizeof(long long));
  if (matrix->result_arr == NULL) {
    exit(1);
  }

  for (long long i = 0; i < rows; i++) {
    matrix->result_arr[i] = 0;
  }

  matrix->counter = rows * columns;
}

void destroy_matrix_utils(matrix_utils_t *matrix) {
  if (pthread_mutex_destroy(matrix->mutex) != 0) {
    exit(1);
  }
  free(matrix->mutex);

  if (sem_destroy(matrix->sem) != 0) {
    exit(1);
  }
  free(matrix->sem);
  free(matrix->result_arr);
}

void create_matrix_info(matrix_worker_info_t *info, matrix_utils_t *matrix,
  long long value, long long time, long long row) {
  if (info == NULL) {
    exit(1);
  }
  
  info->matrix = matrix;
  info->value = value;
  info->time = time;
  info->row = row;
}

void destroy_matrix_info(matrix_worker_info_t **info, long long rows,
  long long columns) {
  for (long long i = 0; i < rows * columns; i++) {
    free(info[i]);
  }
}
runnable_t create_runnable(matrix_worker_info_t *info) {
  runnable_t res;
  res.function = matrix_worker;
  res.arg = info;

  return res;
}

int main() {
  thread_pool_t thpool;
  thread_pool_init(&thpool, THREAD_POOL_SIZE);

  long long rows, columns;

  scanf("%lld%lld", &rows, &columns);

  matrix_utils_t *matrix = malloc(sizeof(matrix_utils_t));
  init_matrix_utils(matrix, rows, columns);
  matrix_worker_info_t **info = malloc(rows * columns * sizeof(matrix_worker_info_t));

  for (long long i = 0; i < rows * columns; i++) {
    long long v, t;
    scanf("%lld%lld", &v, &t);

    info[i] = malloc(sizeof(matrix_worker_info_t));
    create_matrix_info(info[i], matrix, v, t, i / columns);

    defer(&thpool, create_runnable(info[i]));
  }

  if (sem_wait(matrix->sem) != 0) {
    exit(1);
  }

  for (long long i = 0; i < rows; i++) {
    printf("%lld\n", matrix->result_arr[i]);
  }

  destroy_matrix_info(info, rows, columns);
  free(info);
  destroy_matrix_utils(matrix);
  free(matrix);
  thread_pool_destroy(&thpool);

  return 0;
}