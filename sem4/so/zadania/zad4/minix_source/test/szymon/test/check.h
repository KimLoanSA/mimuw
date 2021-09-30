#ifndef CHECK_H
#define CHECK_H

int give_kudos_to_determine_kudos(pid_t pid) {
    int oldprio = givekudos(pid);
    assert(oldprio >= 0);
    assert(oldprio != 0); // jeśli 0 to się nie da określić

    int i = 1, prio;    // już dał jeden kudos (kudo?)
    do {
        prio = givekudos(pid);
        ++i;
    } while (prio == oldprio);
    assert(oldprio == prio + 1);

    if (prio == 0) {
        return 50 - i;
    } else if (prio == 1) {
        return 25 - i;
    } else if (prio == 2) {
        return 10 - i;
    } else {
        assert(0);
    }

    return -1;
}

#endif
