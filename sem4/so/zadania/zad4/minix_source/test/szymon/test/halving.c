// Sprawdza, że przy fork() kudosy są odpowiednio podzielone.
// A tworzy B i C -> C daje B kudosy -> B tworzy D -> C sprawdza liczbę kudosów
// B -> B tworzy E -> E sprawdza liczbę kudosów B

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "check.h"

int main() {
    pid_t pid, pid1;
    int r;
    switch (pid = fork()) {
        case -1:
            assert(0);
        case 0:
            // B
            usleep(100000);

            switch (pid1 = fork()) {
                case -1:
                    assert(0);
                case 0:
                    // D
                    sleep(1);
                    break;
                default:
                    // B
                    usleep(500000);
                    switch (fork()) {
                        case -1:
                            assert(0);
                        case 0:
                            // E
                            assert(give_kudos_to_determine_kudos(pid1) == 7);
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
            switch (fork()) {
                case -1:
                    assert(0);
                case 0:
                    // C
                    for (int i = 0; i < 14; ++i) {
                        r = givekudos(pid);
                        assert(r >= 0);
                    }

                    assert(givekudos(pid) == 2); // 15 kudos

                    usleep(200000);
                    assert(give_kudos_to_determine_kudos(pid) == 8);

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
