#include <unistd.h>
#include <assert.h>
#include "err.h"

int main() {
    pid_t pid, pid1;
    switch (pid = fork()) {
        case -1:
            syserr("fork");
        case 0:
            pid = getppid();
            switch (pid1 = fork()) {
                case -1:
                    syserr("fork");
                case 0:
                    pid1 = getppid();
                    assert(!changeparent());
                    assert(getppid() == pid);
                    assert(getoppid(getpid()) == pid1);
                default:
                    usleep(1000);
            }

            break;
        default:
            assert(getpid() == getoppid(pid));

            for (int i = 0; i < 2; ++i) {
                if (wait(0) == -1)
                    syserr("wait");
            }

            break;
    }

    return 0;
}
