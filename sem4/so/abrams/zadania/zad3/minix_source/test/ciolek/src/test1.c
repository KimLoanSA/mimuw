#include "utils.h"

/*
    A -> B -> C
0)
    C does changeparent

1)  
    B dies, becomes zombie

2)
    C checks B is zombie

3)
    A waits for B

4)
    C checks getoppid == 0
    A waits for C
*/

static int do_A(pid_t A_pid, pid_t B_pid, pid_t C_pid)
{
    wait_event(3);

    assert(wait(NULL) == B_pid);
    say("A collected B");

    assert(wait(NULL) != -1); // wait for C

    return 0;
}

static int do_B(pid_t A_pid, pid_t B_pid, pid_t C_pid)
{
    wait_event(1); // Wait a moment and die

    say("B dies");

    return 0;
}

static int do_C(pid_t A_pid, pid_t B_pid, pid_t C_pid)
{
    say("C assumes B is alive");

    assert(getoppid(C_pid) == B_pid);

    say("C changes parent");

    // Change parent to A
    assert(changeparent() == 0);

    assert(getpid() == C_pid);
    assert(getppid() == A_pid);

    assert(getoppid(C_pid) == B_pid);

    wait_event(2);

    say("C assumes B is zombie");

    assert(getpid() == C_pid);
    assert(getppid() == A_pid);
//    assert(getoppid(C_pid) == 0); // B doesnt run
    assert(getoppid(B_pid) == A_pid); // B exists, a runs

    wait_event(4);

    say("C assumes B is dead and collected");

//    assert(getoppid(C_pid) == 0);
    assert(getoppid(B_pid) == -1 && errno == EINVAL);

    // A is waiting so cannot change
    assert(changeparent() == -1 && errno == EPERM);

    say("OK");

    return 0;   
}

int main()
{
    say("Test 1");

    return fork_three(do_A, do_B, do_C);
}