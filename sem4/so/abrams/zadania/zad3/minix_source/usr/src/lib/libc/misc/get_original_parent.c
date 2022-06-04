#include <lib.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <minix/rs.h>

pid_t getoppid(pid_t pid) {

  message mess;

  endpoint_t pm_ep;
  if (minix_rs_lookup("pm", &pm_ep) != 0) {
    errno = ENOSYS;
    return (-1);
  }

  mess.m_lc_pm_getsid.pid = pid;
  if (_syscall(pm_ep, PM_GETOPPID, &mess) < 0) {
    return (-1);
  }

  return mess.m_lc_pm_getsid.pid;
}