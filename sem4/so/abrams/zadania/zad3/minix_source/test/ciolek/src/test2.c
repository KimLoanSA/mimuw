#include "utils.h"

/*
    A -> B -> C

1)
    B dies, becomes zombie

2)
    C's parent is now <init>
    C cannot change parent

3)
    A waits for B
    fails to wait for C

4)
    C checks that B doesnt exist

5)
    allow C to finish
*/


static int do_A(pid_t A_pid, pid_t B_pid, pid_t C_pid)
{
    // B runs and A runs
    assert(getoppid(B_pid) == A_pid);

    wait_event(3);

    // B exists (zombie) and A runs so getoppid is OK
    assert(getoppid(B_pid) == A_pid);

    assert(wait(NULL) == B_pid); // wait for B
    say("A collected B");

    assert(wait(NULL) == -1); // C is <init>'s now

    // B doesnt exist now
    assert(getoppid(B_pid) == -1 && errno == EINVAL);

    wait_event(5);

    return 0;
}

static int do_B(pid_t A_pid, pid_t B_pid, pid_t C_pid)
{
    assert(getoppid(B_pid) == A_pid);

    wait_event(1); // Wait for a moment and die

    say("B dies");

    return 0;
}

static int do_C(pid_t A_pid, pid_t B_pid, pid_t C_pid)
{
    say("C assumes B is alive");

    assert(getoppid(C_pid) == B_pid);

    wait_event(2);

    say("C assumes B is zombie");

    // B exists (zombie) and parent runs so getoppid succeds
    assert(getoppid(B_pid) == A_pid); 

    // C exists but parent doesnt run so getoppid returns 0
//    assert(getoppid(C_pid) == 0);

    // C's parent is <init> so cannot change
    assert(changeparent() == -1 && errno == EACCES);

    wait_event(4);

    say("C assumes B has been collected by A");

    // B doesnt exist so getoppid fails
    assert(getoppid(B_pid) == -1 && errno == EINVAL);

    // C still exists but B doesn't
//    assert(getoppid(C_pid) == 0);

    say("OK");

    return 0;
}


int main()
{
    say("Test 2");

    return fork_three(do_A, do_B, do_C);
}
