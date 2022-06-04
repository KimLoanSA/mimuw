#include "classification_resolver.h"

#include <stdio.h>
#include <dirent.h>
#include <string.h>

#define BUFFER_SIZE 1024
#define CLASSIFICATION_DIR_PATH "./klasyfikacja/"
#define NO_CLASSIFICATION_DIR_NAME "null"

int should_file_or_dir_be_printed(struct dirent *current_dir) {
  return strncmp(".", current_dir->d_name, 1) != 0
    && strncmp("..", current_dir->d_name, 2) != 0
    && strncmp(NO_CLASSIFICATION_DIR_NAME, current_dir->d_name, 4) != 0;
}

void print_all_subdirs(DIR *dir) {
  struct dirent *current_dir;
  while (current_dir = readdir (dir)) {
    if (should_file_or_dir_be_printed(current_dir) == 1) {
      fprintf(stdout, "- %s\n", current_dir->d_name);
    }
  }
}

void list_all_classification_symbols() {
  DIR *dir = opendir (CLASSIFICATION_DIR_PATH);

  if (dir != NULL) {
    print_all_subdirs(dir);
    closedir(dir);
  } else {
    fprintf(stderr, "Cos poszlo nie tak przy szukaniu klasyfikacji...");
  }
}

void print_file_header(FILE *file) {
  char line[BUFFER_SIZE];
  if (feof(file) == 0) {
    if (fgets(line, BUFFER_SIZE, file)) {
      fprintf(stdout, "- %s", line);
    }
  }
}

void get_final_file_and_print_header(FILE *mapping_file) {
  char final_file_path[BUFFER_SIZE];
  if (feof(mapping_file) == 0) {
    if (fgets(final_file_path, BUFFER_SIZE, mapping_file)) {
      FILE *final_file = fopen(final_file_path, "r");

      if (final_file != NULL) {
        print_file_header(final_file);
        fclose(final_file);
      } else {
        fprintf(stderr, "Cos poszlo nie tak przy szukaniu plikow...");
      }
    }
  }
}

void print_file_header_if_valid(const char *file_path) {
  FILE *mapping_file = fopen(file_path, "r");
  if (mapping_file != NULL) {
    get_final_file_and_print_header(mapping_file);

    fclose(mapping_file);
  } else {
    fprintf(stderr, "Cos poszlo nie tak przy szukaniu plikow...");
  }
}

void print_all_files_headers_in_dir(DIR *dir, const char *dir_path) {
  struct dirent *current_dir;

  while (current_dir = readdir(dir)) {
    if (should_file_or_dir_be_printed(current_dir) == 1) {
      char file_path[3 * BUFFER_SIZE];
      snprintf(file_path, 3 * BUFFER_SIZE, "%s%s", dir_path, current_dir->d_name);

      print_file_header_if_valid(file_path);
    }
  }
}

void list_all_headers_for_given_classification(const char *classification) {
  char dir_path[2 * BUFFER_SIZE];
  snprintf(dir_path, 2 * BUFFER_SIZE, "%s%s/", CLASSIFICATION_DIR_PATH, classification);
  DIR *dir = opendir(dir_path);

  if (dir != NULL) {
    print_all_files_headers_in_dir(dir, dir_path);
    closedir(dir);
  } else {
    fprintf(stderr, "Niepoprawna klasyfikacja!");
  }
}

void list_all_headers_for_classification() {
  fprintf(stdout, "Podaj klasyfikacje: ");
  char classification[BUFFER_SIZE];
  scanf("%1023s", (char *) &classification);

  if (strncmp(NO_CLASSIFICATION_DIR_NAME, classification, 4) == 0) {
    fprintf(stderr, "Niepoprawna klasyfikacja!");
    return;
  }

  list_all_headers_for_given_classification(classification);
}


void list_all_headers_for_no_classification() {
  list_all_headers_for_given_classification(NO_CLASSIFICATION_DIR_NAME);
}


void change_file_classification() {
  fprintf(stdout, "Podaj kredyt: ");
  char credit_name[BUFFER_SIZE];
  scanf("%1023s", (char *) &credit_name);

  fprintf(stdout, "Podaj jego aktualna klasyfikacje: ");
  char old_classification[BUFFER_SIZE];
  scanf("%1023s", (char *) &old_classification);

  fprintf(stdout, "Podaj jego nowa klasyfikacje: ");
  char new_classification[BUFFER_SIZE];
  scanf("%1023s", (char *) &new_classification);

  char old_credit_classification_path[4 * BUFFER_SIZE];
  snprintf(old_credit_classification_path, 4 * BUFFER_SIZE, "%s%s/%s",
    CLASSIFICATION_DIR_PATH, old_classification, credit_name);

  char new_credit_classification_path[4 * BUFFER_SIZE];
  snprintf(new_credit_classification_path, 4 * BUFFER_SIZE, "%s%s/%s",
    CLASSIFICATION_DIR_PATH, new_classification, credit_name);

  int rename_val = rename(old_credit_classification_path, new_credit_classification_path);

  if(rename_val == 0) {
    fprintf(stdout, "Zrobione!");
  } else {
    fprintf(stderr, "Operacja sie nie powiodla! sprawdz podane dane");
  }
}