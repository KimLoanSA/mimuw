#include <lib.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <minix/rs.h>

int givekudos(pid_t pid) {
  message mess;
  memset(&mess, 0, sizeof(mess));

  endpoint_t pm_ep;
  if (minix_rs_lookup("pm", &pm_ep) != 0) {
    errno = ENOSYS;
    return -1;
  }

  mess.m_lc_pm_getsid.pid = pid;
  if (_syscall(pm_ep, PM_GIVEKUDOS, &mess) < 0) {
    return -1;
  }

  return mess.m_m1.m1i1;
}