#ifndef MIMUW_S5_BSK_ZADANIE_3_FUNCTIONS_TO_HANDLE_H
#define MIMUW_S5_BSK_ZADANIE_3_FUNCTIONS_TO_HANDLE_H

#include "template/commands_handler.h"

//#include "example/example_function.h"
#include "impl/classification_resolver.h"

command_t commands_to_handle[] = {
//  example hello world:
//  {
//    .description = "Example function - hello world",
//    .requires_manager = 0,
//    .handled_function = hello_world
//  },

//   add here new functions...

  {
    .description = "Wypisz wszystkie symbole klasyfikacyjne",
    .requires_manager = 0,
    .handled_function = list_all_classification_symbols,
  },
  {
    .description = "Wypisz naglowki kredytow od danej klasyfikacji",
    .requires_manager = 0,
    .handled_function = list_all_headers_for_classification,
  },
  {
    .description = "Wypisz naglowki kredytow bez klasyfikacji",
    .requires_manager = 0,
    .handled_function = list_all_headers_for_no_classification,
  },
  {
    .description = "Zmiana klasyfikacji kredytu",
    .requires_manager = 1,
    .handled_function = change_file_classification,
  },

};

int commands_to_handle_size = sizeof(commands_to_handle) / sizeof(command_t);

#endif //MIMUW_S5_BSK_ZADANIE_3_FUNCTIONS_TO_HANDLE_H
