#include <minix/drivers.h>
#include <minix/chardriver.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <minix/ds.h>
#include <minix/ioctl.h>
#include <sys/ioc_dfa.h>

static int dfa_open(devminor_t minor, int access, endpoint_t user_endpt);
static int dfa_close(devminor_t minor);
static ssize_t dfa_read(devminor_t minor, u64_t position, endpoint_t endpt, cp_grant_id_t grant, size_t size, int flags, cdev_id_t id);
static ssize_t dfa_write(devminor_t minor, u64_t pos, endpoint_t ep, cp_grant_id_t gid, size_t size, int flags, cdev_id_t id);
static int dfa_ioctl(devminor_t minor, unsigned long request, endpoint_t endpt, cp_grant_id_t grant, int flags, endpoint_t user_endpt, cdev_id_t id);

static int dfa_ioctl_dfaiocreset(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt);
static int dfa_ioctl_dfaiocadd(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt);
static int dfa_ioctl_dfaiocaccept(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt);
static int dfa_ioctl_dfaiocreject(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt);

static void make_move(char letter);
static void create_id_string_for_index(int i);
static int index_dfa(char i, char j);
static void sef_init_fresh_call();

static void sef_local_startup(void);
static int sef_cb_init(int type, sef_init_info_t *info);
static int sef_cb_lu_state_save(int);
static int lu_state_restore(void);

static struct chardriver hello_tab =
  {
    .cdr_open	= dfa_open,
    .cdr_close	= dfa_close,
    .cdr_read	= dfa_read,
    .cdr_write	= dfa_write,
    .cdr_ioctl  = dfa_ioctl
  };

static const int DFA_SIZE = 256;
static const int DFA_BASE = 128;

static const char YES_ANSWER = 89;
static const char NO_ANSWER = 78;

static const int BUFFER_SIZE = 128;

static const int dfa_q0 = DFA_BASE;

static char transitions[DFA_SIZE * DFA_SIZE];
static char ending_state[DFA_SIZE];

static int dfa_state = dfa_q0;


static int dfa_open(devminor_t UNUSED(minor), int UNUSED(access), endpoint_t UNUSED(user_endpt)) {
  return OK;
}


static int dfa_close(devminor_t UNUSED(minor)) {
  return OK;
}


static ssize_t dfa_read(devminor_t UNUSED(minor), u64_t UNUSED(position), endpoint_t endpt, cp_grant_id_t grant,
  size_t size, int UNUSED(flags), cdev_id_t UNUSED(id)) {
  int ret;
  char answer = ending_state[dfa_state];

  if ((ret = sys_safememset(endpt, grant, 0, answer, size)) != OK) {
    return ret;
  }

  return size;
}


static ssize_t dfa_write(devminor_t UNUSED(minor), u64_t UNUSED(pos), endpoint_t ep, cp_grant_id_t gid,
  size_t size, int UNUSED(flags), cdev_id_t UNUSED(id)) {
  int ret;
  int dev_size = size;
  int offset = 0;
  char buffer[BUFFER_SIZE];

  for (int i = 0; i < size / BUFFER_SIZE; i++) {
    if ((ret = sys_safecopyfrom(ep, gid, offset, (vir_bytes) buffer, BUFFER_SIZE)) != OK) {
      return ret;
    }

    for (int j = 0; j < BUFFER_SIZE; j++) {
      make_move(buffer[j]);
    }

    dev_size -= BUFFER_SIZE;
    offset += BUFFER_SIZE;
  }

  if (dev_size > 0) {
    if ((ret = sys_safecopyfrom(ep, gid, offset, (vir_bytes) buffer, dev_size)) != OK) {
      return ret;
    }

    for (int i = 0; i < dev_size; i++) {
      make_move(buffer[i]);
    }
  }

  return size;
}

static void make_move(char letter) {
  dfa_state = (int) transitions[index_dfa(dfa_state - DFA_BASE, letter)] + DFA_BASE;
}


static int dfa_ioctl(devminor_t UNUSED(minor), unsigned long request, endpoint_t endpt, cp_grant_id_t grant,
  int UNUSED(flags), endpoint_t user_endpt, cdev_id_t UNUSED(id)) {
  switch(request) {
    case DFAIOCRESET:
      return dfa_ioctl_dfaiocreset(request, endpt, grant, user_endpt);
    case DFAIOCADD:
      return dfa_ioctl_dfaiocadd(request, endpt, grant, user_endpt);
    case DFAIOCACCEPT:
      return dfa_ioctl_dfaiocaccept(request, endpt, grant, user_endpt);
    case DFAIOCREJECT:
      return dfa_ioctl_dfaiocreject(request, endpt, grant, user_endpt);
    default:
      return ENOTTY;
  }
}

static int dfa_ioctl_dfaiocreset(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt) {
  dfa_state = dfa_q0;

  return OK;
}

static int dfa_ioctl_dfaiocadd(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt) {
  char transition[3];

  int return_code = sys_safecopyfrom(endpt, grant, 0, (vir_bytes) transition, 3);

  if (return_code == OK) {
    transitions[index_dfa(transition[0], transition[1])] = transition[2];
  }

  return return_code;
}

static int index_dfa(char i, char j) {
  return ((int) i + DFA_BASE) * DFA_SIZE + (int) j + DFA_BASE;
}

static int dfa_ioctl_dfaiocaccept(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt) {
  char new_ending_state;

  int return_code = sys_safecopyfrom(endpt, grant, 0, (vir_bytes) &new_ending_state, 1);

  if (return_code == OK) {
    ending_state[(int) new_ending_state + DFA_BASE] = YES_ANSWER;
  }

  return return_code;
}

static int dfa_ioctl_dfaiocreject(unsigned long request, endpoint_t endpt, cp_grant_id_t grant, endpoint_t user_endpt) {
  char old_ending_state;

  int return_code = sys_safecopyfrom(endpt, grant, 0, (vir_bytes) &old_ending_state, 1);

  if (return_code == OK) {
    ending_state[(int) old_ending_state + DFA_BASE] = NO_ANSWER;
  }

  return return_code;
}


static int sef_cb_lu_state_save(int UNUSED(state)) {
  ds_publish_mem("dfa_transitions", transitions, DFA_SIZE * DFA_SIZE, DSF_OVERWRITE);

  ds_publish_mem("ending_state", ending_state, DFA_SIZE, DSF_OVERWRITE);

  u32_t value = (unsigned int) dfa_state;
  ds_publish_u32("dfa_state", value, DSF_OVERWRITE);

  return OK;
}

static int lu_state_restore() {

  u32_t size = DFA_SIZE * DFA_SIZE;
  ds_retrieve_mem("dfa_transitions", transitions, &size);
  ds_delete_mem("dfa_transitions");

  size = DFA_SIZE;
  ds_retrieve_mem("ending_state", ending_state, &size);
  ds_delete_mem("ending_state");

  u32_t value;
  ds_retrieve_u32("dfa_state", &value);
  ds_delete_u32("dfa_state");
  dfa_state = (int) value;

  return OK;
}


static void sef_local_startup() {
  sef_setcb_init_fresh(sef_cb_init);
  sef_setcb_init_lu(sef_cb_init);
  sef_setcb_init_restart(sef_cb_init);

  sef_setcb_lu_prepare(sef_cb_lu_prepare_always_ready);
  sef_setcb_lu_state_isvalid(sef_cb_lu_state_isvalid_standard);
  sef_setcb_lu_state_save(sef_cb_lu_state_save);

  sef_startup();
}

static int sef_cb_init(int type, sef_init_info_t *UNUSED(info)) {
  int do_announce_driver = TRUE;

  switch(type) {
    case SEF_INIT_FRESH:
      sef_init_fresh_call();
      break;

    case SEF_INIT_LU:
      lu_state_restore();
      do_announce_driver = FALSE;
      break;

    case SEF_INIT_RESTART:
      break;
  }

  if (do_announce_driver) {
    chardriver_announce();
  }

  return OK;
}

static void sef_init_fresh_call() {
  dfa_state = dfa_q0;

  for (int i = 0; i < DFA_SIZE * DFA_SIZE + 1; i++) {
    transitions[i] = 0;
  }

  for (int i = 0; i < DFA_SIZE; i++) {
    ending_state[i] = NO_ANSWER;
  }
}


int main(void) {
  sef_local_startup();

  chardriver_task(&hello_tab);

  return OK;
}