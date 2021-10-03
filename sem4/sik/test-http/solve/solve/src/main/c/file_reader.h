#ifndef MIMUW_S4_SIK_TESTHTTP_FILE_READER_H
#define MIMUW_S4_SIK_TESTHTTP_FILE_READER_H

typedef struct file_reader_persistence file_reader_persistence_t;

file_reader_persistence_t *init_file_reader(const char *file_name);
const char *get_next_line(file_reader_persistence_t *file_reader_persistence);
int has_ended(file_reader_persistence_t *file_reader_persistence);
void delete_file_reader(file_reader_persistence_t *file_reader_persistence);

#endif //MIMUW_S4_SIK_TESTHTTP_FILE_READER_H
