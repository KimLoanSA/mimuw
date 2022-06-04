// Sprawdza, że nie można dać samemu sobie kudosów.
// A tworzy B -> B sprawdza, że nie może dać sobie kudosów -> A tworzy C
// -> C sprawdza, że B ma odpowiednią liczbę kudosów

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "check.h"

int main() {
    pid_t pid;
    switch (pid = fork()) {
        case -1:
            assert(0);
        case 0:
            // B
            assert(givekudos(getpid()) == -1);
            assert(errno == EPERM);
            usleep(100000);
            break;
        default:
            // A
            switch (fork()) {
                case -1:
                    assert(0);
                case 0:
                    // C
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

