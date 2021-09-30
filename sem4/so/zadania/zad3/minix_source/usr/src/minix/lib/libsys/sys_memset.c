#include "syslib.h"

int sys_memset(endpoint_t who, unsigned long pattern,
	phys_bytes base, phys_bytes bytes)
{
/* Zero a block of data.  */
  message mess;

  if (bytes == 0L) return(OK);

  mess.m_lsys_krn_sys_memset.base = base;
  mess.m_lsys_krn_sys_memset.count = bytes;
  mess.m_lsys_krn_sys_memset.pattern = pattern;
  mess.m_lsys_krn_sys_memset.process = who;

  return(_kernel_call(SYS_MEMSET, &mess));
}

