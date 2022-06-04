#ifndef MIMUW_S5_BSK_ZADANIE_3_COMMANDS_HANDLER_H
#define MIMUW_S5_BSK_ZADANIE_3_COMMANDS_HANDLER_H

#include "auth.h"

typedef void (*handled_function_t)();

typedef struct command {
  const char *description;
  int requires_manager;
  handled_function_t handled_function;
} command_t;

typedef struct command_handler_persistence command_handler_persistence_t;

command_handler_persistence_t *init_command_handler_persistence(user_type_t user_type);
void handle_commands(command_handler_persistence_t *command_handler_persistence);
void delete_command_handler_persistence(command_handler_persistence_t *command_handler_persistence);

#endif //MIMUW_S5_BSK_ZADANIE_3_COMMANDS_HANDLER_H
