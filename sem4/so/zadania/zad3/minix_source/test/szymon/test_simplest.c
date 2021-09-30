#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "err.h"

int main() {
    pid_t pid;
    switch (pid = fork()) {
        case -1:
            syserr("fork");
        case 0:
            assert(getppid() == getoppid(getpid()));
            break;
        default:
            usleep(1000);
            assert(getpid() == getoppid(pid));

            if (wait(0) == -1)
                syserr("wait");

            break;
    }

    return 0;
}
