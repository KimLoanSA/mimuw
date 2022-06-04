#ifndef MIMUW_S5_BSK_ZADANIE_3_AUTH_H
#define MIMUW_S5_BSK_ZADANIE_3_AUTH_H

typedef enum {
  MANAGER,
  WORKER
} user_type_t;

typedef struct auth_persistence auth_persistence_t;

auth_persistence_t *init_auth();
user_type_t log_user_and_check_type(auth_persistence_t *auth_persistence);
void delete_auth(auth_persistence_t *auth_persistence);

#endif //MIMUW_S5_BSK_ZADANIE_3_AUTH_H
