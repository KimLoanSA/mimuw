#ifndef INC_THREAD_H
#define INC_THREAD_H

typedef struct {
  int *value;
  int count;
} thread_data_t;

void *inc_thread(void *);

#endif