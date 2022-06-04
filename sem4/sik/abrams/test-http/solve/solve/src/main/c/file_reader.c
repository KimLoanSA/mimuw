#include "file_reader.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>

#include "safe_memory.h"
#include "logger.h"

static void update_end_if_needed(file_reader_persistence_t *file_reader_persistence, ssize_t read_line_size);
static char *remove_new_line(char *line, ssize_t line_size);

typedef struct file_reader_persistence {
  FILE *file;
  int is_end;
} file_reader_persistence_t;

static void open_file_or_exit(file_reader_persistence_t *file_reader_persistence, const char *file_name);

file_reader_persistence_t *init_file_reader(const char *file_name) {
  file_reader_persistence_t *file_reader_persistence;
  file_reader_persistence = malloc_or_exit_on_failure(sizeof(file_reader_persistence_t));

  open_file_or_exit(file_reader_persistence, file_name);
  file_reader_persistence->is_end = 0;

  return file_reader_persistence;
}

void open_file_or_exit(file_reader_persistence_t *file_reader_persistence, const char *file_name) {
  file_reader_persistence->file = fopen(file_name, "r");

  if (file_reader_persistence->file == NULL) {
    log_and_exit(1, "File opening error");
  }
}

const char *get_next_line(file_reader_persistence_t *file_reader_persistence) {
  char *line = NULL;
  size_t line_size = 0;

  ssize_t read_line_size = getline(&line, &line_size, file_reader_persistence->file);
  update_end_if_needed(file_reader_persistence, read_line_size);

  return remove_new_line(line, read_line_size);
}

void update_end_if_needed(file_reader_persistence_t *file_reader_persistence, ssize_t read_line_size) {
  if (read_line_size <= 0) {
    file_reader_persistence->is_end = 1;
  }
}

static char *remove_new_line(char *line, ssize_t line_size) {
  if (line_size > 0 && line[line_size - 1] == '\n') {
    line[line_size - 1] = '\0';
  }

  return line;
}

int has_ended(file_reader_persistence_t *file_reader_persistence) {
  return file_reader_persistence->is_end;
}

void delete_file_reader(file_reader_persistence_t *file_reader_persistence) {
  fclose(file_reader_persistence->file);

  free(file_reader_persistence);
}