#include <lib.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <minix/rs.h>

int changeparent(void) {

  message mess;

  endpoint_t pm_ep;
  if (minix_rs_lookup("pm", &pm_ep) != 0) {
    errno = ENOSYS;
    return (-1);
  }

  if (_syscall(pm_ep, PM_CHANGE_PARENT, &mess) < 0) {
    return (-1);
  }

  return 0;
}