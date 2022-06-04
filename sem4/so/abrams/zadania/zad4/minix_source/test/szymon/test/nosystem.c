// Sprawdza, że nie można dać kudosów procesowi nie zarządzanemu przez sched
// (dokładniej rs).
// TODO sprawdzić czy zawsze te samo pid

#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "check.h"

int main() {
    assert(givekudos(4) == -1);
    assert(errno == EINVAL);
}

