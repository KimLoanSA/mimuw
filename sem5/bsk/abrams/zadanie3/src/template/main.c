#include <signal.h>
#include <stdlib.h>

#include "auth.h"
#include "commands_handler.h"

#define TEXT_BUFFER_SIZE 1024

auth_persistence_t *auth_persistence = NULL;

void sigint_handler() {
  delete_auth(auth_persistence);

  exit(0);
}

int main() {
  signal(SIGINT, sigint_handler);

  auth_persistence = init_auth();
  user_type_t user_type =
    log_user_and_check_type(auth_persistence);

  command_handler_persistence_t *command_handler_persistence =
    init_command_handler_persistence(user_type);

  handle_commands(command_handler_persistence);

  delete_command_handler_persistence(command_handler_persistence);
  delete_auth(auth_persistence);

  return 0;
}