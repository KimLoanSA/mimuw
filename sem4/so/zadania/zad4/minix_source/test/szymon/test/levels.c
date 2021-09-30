// Sprawdza, że poziomy kudosów odpowiadają odpowiednim priorytetom.
// A tworzy B i C -> C daje B kudosy i sprawdza priorytety B.

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>

int main() {
    pid_t pid;
    int r;
    switch (pid = fork()) {
        case -1:
            assert(0);
        case 0:
            // B
            usleep(100000);
            break;
        default:
            // A
            switch (fork()) {
                case -1:
                    assert(0);
                case 0:
                    // C 
                    for (int i = 0; i < 100;) {
                        r = givekudos(pid);
                        assert(r >= 0);
                        ++i;
                        if (i < 10) {
                            assert(r == 3);
                        } else if (i < 25) {
                            assert(r == 2);
                        } else if (i < 50) {
                            assert(r == 1);
                        } else {
                            assert(r == 0);
                        }
                    }
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

