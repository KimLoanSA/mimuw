#include "program_arguments_validator.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "logger.h"
#include "address_resolver.h"

static int validate_number_of_arguments(size_t required_number_of_arguments, size_t number_of_arguments);
static int validate_port(const char *string);
static void print_usage_and_exit(const char *program_name, const char *usage);
static int are_arguments_correct(size_t number_of_arguments, const char **arguments);

static const size_t REQUIRED_NUMBER_OF_ARGUMENTS = 4;
static const char *USAGE = "<adres połączenia>:<port> <plik ciasteczek> <testowany adres http>";

void validate_program_arguments(const size_t number_of_arguments, const char **arguments) {
  if (are_arguments_correct(number_of_arguments, arguments) == 0) {
    print_usage_and_exit(arguments[0], USAGE);
  }
}

int are_arguments_correct(const size_t number_of_arguments, const char **arguments) {
  return validate_number_of_arguments(REQUIRED_NUMBER_OF_ARGUMENTS, number_of_arguments)
      && validate_port(arguments[1]);
}

int validate_number_of_arguments(const size_t required_number_of_arguments, const size_t number_of_arguments) {
  return required_number_of_arguments == number_of_arguments;
}

int validate_port(const char *string) {
  const int max_port_value = 65535;
  const long port = get_port_long(string);

  return port > 0
    && port < max_port_value;
}


void print_usage_and_exit(const char *program_name, const char *usage) {
  log_and_exit(1, "Usage: %s %s", program_name, usage);
}