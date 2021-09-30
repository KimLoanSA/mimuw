// Sprawdza, że nie da się dać kudosów dziadkowi.
// A tworzy B -> B tworzy C -> C tworzy D -> D sprawdza, że nie da się dać
// kudosów B -> A tworzy E -> E sprawdza, że B ma odpowiednią liczbę kudosów

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "check.h"

int main() {
    pid_t pid, pid1;
    switch (pid = fork()) {
        case -1:
            assert(0);
        case 0:
            // B
            pid = getpid();
            switch(pid1 = fork()) {
                case -1:
                    assert(0);
                case 0:
                    // C
                    switch(fork()) {
                        case -1:

                        case 0:
                            // D
                            assert(givekudos(pid) == -1);
                            assert(errno == EPERM);
                            usleep(200000);
                            break;
                        default:
                            if (wait(0) == -1)
                                assert(0);
                            break;
                    }
                    break;
                default:
                    // B
                    if (wait(0) == -1)
                        assert(0);

                    break;
            }

            break;
        default:
            // A
            switch (fork()) {
                case -1:
                    assert(0);
                case 0:
                    // D
                    usleep(100000);
                    assert(give_kudos_to_determine_kudos(pid) == 0);
                    break;
                default:
                    // A
                    for (int i = 0; i < 2; ++i) {
                        if (wait(0) == -1)
                            assert(0);
                    }

                    break;
            }

            break;
    }

    return 0;
}

