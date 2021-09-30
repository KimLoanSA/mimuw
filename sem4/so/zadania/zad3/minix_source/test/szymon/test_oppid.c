#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

int main() { // run this test with &, so that shell doesn't wait for it

  pid_t par_pid = getppid();
  
  assert(getoppid(getpid()) == par_pid);
  assert(getoppid(0) == -1);
  assert(errno == EINVAL);
  assert(getoppid(-123) == -1);
  assert(errno == EINVAL);
  assert(getoppid(100000000) == -1);
  assert(errno == EINVAL);

  assert(getoppid(108) == -1); // *
  assert(errno == EINVAL);

  assert(getoppid(1) == 1);

  // cron proccess
  assert(getoppid(165) == 108); // *

  // processes started by rs
  assert(getoppid(5) == 4);
  assert(getoppid(7) == 4);
  assert(getoppid(8) == 4);
  assert(getoppid(6) == 4);
  assert(getoppid(9) == 4);
  assert(getoppid(6) == 4);

  // rs
  assert(getoppid(4) == 1);
    
  return 0;
}
