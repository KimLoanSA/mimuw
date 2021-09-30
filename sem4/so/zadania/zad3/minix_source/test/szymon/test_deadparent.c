#include <unistd.h>
#include <assert.h>
#include "err.h"

int main() {
    pid_t pid;
    switch (pid = fork()) {
        case -1:
            syserr("fork");
        case 0:
            pid = getppid();
            assert(getoppid(getpid()) == pid);
            usleep(2000);
            assert(getoppid(getpid()) == pid || getoppid(getpid()) == 0);
            break;
        default:
            assert(getpid() == getoppid(pid));
            usleep(1000);

            break;
    }

    return 0;
}
