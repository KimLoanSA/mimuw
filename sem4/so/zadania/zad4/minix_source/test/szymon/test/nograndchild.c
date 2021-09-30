// Sprawdza, że nie da się dać kudosów wnukowi.
// A tworzy B -> B tworzy C -> C tworzy D -> B sprawdza, że nie da się dać
// kudosów D -> C tworzy E -> E sprawdza, że D ma odpowiednią liczbę kudosów
// TODO TODO TODO

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "check.h"

int main() {
    pid_t pid, pid1, pid2;
    int pipe_dsc[2];

    assert(pipe(pipe_dsc) != -1);

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
                    assert(close(pipe_dsc[0]) != -1);
                    switch(pid2 = fork()) {
                        case -1:
                            assert(0);
                        case 0:
                            // D
                            usleep(100000);
                            break;
                        default:
                            // C
                            write(pipe_dsc[1], &pid2, sizeof pid2);

                            switch (fork()) {
                                case -1:
                                    assert(0);
                                case 0:
                                    // E
                                    assert(give_kudos_to_determine_kudos(pid2) == 0);
                                    break;
                                default:
                                    // C
                                    for (int i = 0; i < 2; ++i) {
                                        if (wait(0) == -1)
                                            assert(0);
                                    }
                                    break;
                            }
                    }

                    break;
                default:
                    // B
                    assert(close(pipe_dsc[1]) != -1);
                    read(pipe_dsc[0], &pid2, sizeof pid2);

                    assert(givekudos(pid2) == -1);
                    assert(errno == EPERM);

                    if (wait(0) == -1)
                        assert(0);
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

