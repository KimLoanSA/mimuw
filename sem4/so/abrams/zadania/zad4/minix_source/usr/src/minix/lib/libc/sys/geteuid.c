#include <sys/cdefs.h>
#include "namespace.h"
#include <lib.h>

#include <string.h>
#include <unistd.h>

uid_t geteuid(void)
{
  message m;

  memset(&m, 0, sizeof(m));
  /* POSIX says that this function is always successful and that no
   * return value is reserved to indicate an error.  Minix syscalls
   * are not always successful and Minix returns the unreserved value
   * (uid_t) -1 when there is an error.
   */
  if (_syscall(PM_PROC_NR, PM_GETUID, &m) < 0) return(-1);
  return(m.m_pm_lc_getuid.euid);
}
