#include "auth.h"

#include "safe_memory.h"

#include <security/pam_appl.h>
#include <security/pam_misc.h>
#include <stdio.h>

#define MANAGER_PAM_FILE "bsk_3_manager"
#define WORKER_PAM_FILE "bsk_3_worker"

#define MANAGER_INPUT_VALUE 'm'
#define WORKER_INPUT_VALUE 'w'

static struct pam_conv login_conv = {
  misc_conv,
  NULL
};

typedef struct auth_persistence {
  pam_handle_t *pam_handle;
  user_type_t user_type;
} auth_persistence_t;


void init_pam_handle(auth_persistence_t *auth_persistence, const char *pam_file) {
  int ret_val = pam_start(pam_file, NULL, &login_conv, &auth_persistence->pam_handle);

  if (auth_persistence->pam_handle == NULL || ret_val != PAM_SUCCESS) {
    fprintf(stderr, "pam_start fail (code: %d)", ret_val);
    exit(1);
  }
}

user_type_t ask_user_for_type() {
  fprintf(stdout, "Czy chcesz sie zalogowac jako manager czy pracownik?\n"
                  "(%c - jesli jako manager, %c - jesli jako pracownik)\n",
                  MANAGER_INPUT_VALUE, WORKER_INPUT_VALUE);

  char user_type_input[2];
  scanf("%1s", (char *) &user_type_input);

  if (user_type_input[0] == MANAGER_INPUT_VALUE) {
    return MANAGER;
  }

  return WORKER;
}

auth_persistence_t *init_auth() {
  auth_persistence_t *auth_persistence = malloc_or_exit_on_failure(sizeof(auth_persistence_t));
  auth_persistence->pam_handle = NULL;
  auth_persistence->user_type = ask_user_for_type();

  if (auth_persistence->user_type == MANAGER) {
    init_pam_handle(auth_persistence, MANAGER_PAM_FILE);
  } else {
    init_pam_handle(auth_persistence, WORKER_PAM_FILE);
  }

  return auth_persistence;
}


void validate_user(auth_persistence_t *auth_persistence) {
  int ret_val = pam_acct_mgmt(auth_persistence->pam_handle, 0);

  if (ret_val != PAM_SUCCESS) {
    fprintf(stdout, "Niestety nie masz zadeklarowanych uprawnien, \n"
                    "zaloguj sie ponownie z poprawnymi\n");
    exit(3);
  }
}

void log_user(auth_persistence_t *auth_persistence) {
  int ret_val = pam_authenticate(auth_persistence->pam_handle, 0);

  if (ret_val != PAM_SUCCESS) {
    fprintf(stderr, "Chyba się nie udało!\n");
    exit(2);
  }
}

user_type_t log_user_and_check_type(auth_persistence_t *auth_persistence) {
  log_user(auth_persistence);
  validate_user(auth_persistence);

  fprintf(stdout, "Zalogowales sie jako %s\n",
    auth_persistence->user_type == MANAGER ? "Manager" : "Pracownik");

  return auth_persistence->user_type;
}


void delete_auth(auth_persistence_t *auth_persistence) {
  pam_end(auth_persistence->pam_handle, PAM_SUCCESS);

  free(auth_persistence);
}