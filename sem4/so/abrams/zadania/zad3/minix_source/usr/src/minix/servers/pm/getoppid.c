#include "pm.h"
#include <minix/callnr.h>
#include <minix/endpoint.h>
#include <limits.h>
#include <minix/com.h>
#include <signal.h>
#include "mproc.h"

int do_getoppid(void) {

  register struct mproc *rmp = mp;

  pid_t calling_pid = m_in.m_lc_pm_getsid.pid;

  struct mproc *calling_process_mproc = find_proc(calling_pid);

  if (calling_process_mproc == NULL) {
    return EINVAL;
  }

  pid_t calling_process_original_parend_pid = calling_process_mproc->mp_original_parent_pid;
  rmp->mp_reply.m_lc_pm_getsid.pid = calling_process_original_parend_pid;

  return 0;
}