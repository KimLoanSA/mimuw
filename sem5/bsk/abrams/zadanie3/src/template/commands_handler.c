#include "commands_handler.h"

#include <stdio.h>

#include "../commands_to_handle.h"
#include "safe_memory.h"

#define EXIT_COMMAND_ID (-1)

typedef struct command_handler_persistence {
  user_type_t user_type;
} command_handler_persistence_t;


command_handler_persistence_t *init_command_handler_persistence(user_type_t user_type) {
  command_handler_persistence_t *command_handler_persistence = malloc_or_exit_on_failure(sizeof(command_handler_persistence_t));
  command_handler_persistence->user_type = user_type;

  return command_handler_persistence;
}


void handle_command(command_handler_persistence_t *command_handler_persistence, int command_id) {
  if (command_id < 0 || command_id >= commands_to_handle_size) {
    fprintf(stderr, "Niepoprawne id komendy!");
    return;
  }

  command_t current_command = commands_to_handle[command_id];

  if (current_command.requires_manager && command_handler_persistence->user_type != MANAGER) {
    fprintf(stderr, "Nie mozesz wykonac tej komendy!");
    return;
  }

  current_command.handled_function();
}

int should_command_be_printed(command_handler_persistence_t *command_handler_persistence, command_t command) {
  return command.requires_manager == 0
    || (command.requires_manager == 1 && command_handler_persistence->user_type == MANAGER);
}


void print_help(command_handler_persistence_t *command_handler_persistence) {
  fprintf(stdout, "Dostepne komendy:\n");

  for (int i = 0; i < commands_to_handle_size; i++) {
    command_t current_command = commands_to_handle[i];
    if (should_command_be_printed(command_handler_persistence, current_command)) {
      fprintf(stdout, "[%d] - %s\n", i, current_command.description);
    }
  }

  fprintf(stdout, "[%d] - wyjscie\n", EXIT_COMMAND_ID);
}

void handle_commands(command_handler_persistence_t *command_handler_persistence) {
  print_help(command_handler_persistence);

  int command_id = -1;
  scanf("%d", &command_id);

  while (command_id != EXIT_COMMAND_ID) {
    handle_command(command_handler_persistence, command_id);

    fprintf(stdout, "\n\n");
    int scanf_result = scanf("%d", &command_id);

    if (scanf_result < 0) {
      fprintf(stderr, "cos zle podano...");
      return;
    }
  }
}


void delete_command_handler_persistence(command_handler_persistence_t *command_handler_persistence) {
  free(command_handler_persistence);
}


