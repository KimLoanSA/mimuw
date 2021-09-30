// Sprawdza, że nie da się dać kudosów dziecku.
// A tworzy B -> B tworzy C -> B sprawdza, że nie da się dać kudosów C
// -> B tworzy D -> D sprawdza, że C ma odpowiednią liczbę kudosów

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
            switch(pid1 = fork()) {
                case -1:
                    assert(0);
                case 0:
                    // C
                    usleep(100000);
                    break;
                default:
                    // B
                    assert(givekudos(pid1) == -1);
                    assert(errno == EPERM);

                    switch(fork()) {
                        case -1:
                            assert(0);
                        case 0:
                            // D
                            assert(give_kudos_to_determine_kudos(pid1) == 0);
                            break;
                        default:
                            // B
                            for (int i = 0; i < 2; ++i) {
                                if (wait(0) == -1)
                                    assert(0);
                            }
                    }

                    break;
            }

            break;
        default:
            // A
            if (wait(0) == -1)
                assert(0);
            break;
    }

    return 0;
}

