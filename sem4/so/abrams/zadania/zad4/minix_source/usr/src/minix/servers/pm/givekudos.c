#include "pm.h"
#include <minix/callnr.h>
#include <minix/endpoint.h>
#include <limits.h>
#include <minix/com.h>
#include <signal.h>
#include "mproc.h"

static int is_parent_of_process(struct mproc *process, struct mproc *possible_parent_process);
static int get_priority_for_kudos(int kudos);

int do_givekudos(void) {

  register struct mproc *rmp = mp;

  pid_t target_process_pid = m_in.m_lc_pm_getsid.pid;
  struct mproc *target_process_mproc = find_proc(target_process_pid);

  if (target_process_mproc == NULL) {
    return EINVAL;
  }

  if (is_parent_of_process(rmp, target_process_mproc) == 1
    || is_parent_of_process(target_process_mproc, rmp) == 1) {
    return EPERM;
  }

  target_process_mproc->number_of_kudos++;

  sched_nice(target_process_mproc, target_process_mproc->number_of_kudos);

  rmp->mp_reply.m_m1.m1i1 = get_priority_for_kudos(target_process_mproc->number_of_kudos);

  return OK;
}

int is_parent_of_process(struct mproc *process, struct mproc *possible_parent_process) {
  if (process->mp_pid == 1) {
    return 0;
  }

  if (process->mp_pid == possible_parent_process->mp_pid) {
    return 1;
  }

  return is_parent_of_process(&mproc[process->mp_parent], possible_parent_process);
}

int get_priority_for_kudos(int kudos) {
  if (kudos < 10) {
    return 3;
  }

  if (kudos < 25) {
    return 2;
  }

  if (kudos < 50) {
    return 1;
  }

  return 0;
}