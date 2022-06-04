#include "pm.h"
#include <minix/callnr.h>
#include <minix/endpoint.h>
#include <limits.h>
#include <minix/com.h>
#include <signal.h>
#include "mproc.h"

int do_changeparent(void) {

  register struct mproc *rmp = mp;

  pid_t my_pid = rmp->mp_pid;
  if (my_pid == 1) {
    return EACCES;
  }

  pid_t parent_pid = mproc[rmp->mp_parent].mp_pid;
  if (parent_pid == 1) {
    return EACCES;
  }

  if (mproc[rmp->mp_parent].mp_flags & WAITING) {
    return EPERM;
  }

  int grand_parent_index = mproc[rmp->mp_parent].mp_parent;
  rmp->mp_parent = grand_parent_index;

  return OK;
}