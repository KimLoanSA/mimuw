#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/wait.h>

#define WAIT_STEP 1 // time to sleep between events

// sleeps for milliseconds
static void msleep(int milliseconds);

// Waits for given event
// Events go 1 -> 2 -> 3 -> ...
// With equal time gaps
static void wait_event(int event_num);

// Outputs string to stdout + newline + fflush
static void say(const char* str);

typedef int(*fork3func)(pid_t A_pid, pid_t B_pid, pid_t C_pid);

//  Forks starting at A and does A -> B -> C
//  and calls fork3func on each of them
//  in func_A C_pid is undefined
static int fork_three(fork3func func_A, fork3func func_B, fork3func func_C);

// syserr from labs
static void syserr(const char *fmt, ...);



static void msleep(int milliseconds)
{
    if(milliseconds > 1000)
    {
        if(sleep(milliseconds/1000) != 0)
            syserr("sleep");
    }

    if(usleep((milliseconds%1000) * 1000) != 0)
        syserr("usleep");
}

static void wait_event(int event_num)
{   
    static int cur_event = 0;
    static const int event_time_gap_in_ms = 2000;

    if(event_num < cur_event)
        return;

    msleep((event_num - cur_event) * event_time_gap_in_ms);
    cur_event = event_num;
}

static void say(const char* str)
{
    if(printf("%s\n", str) < 0)
        syserr("printf");

    if(fflush(stdout) != 0)
        syserr("fflush");
}

static int fork_three(fork3func func_A, fork3func func_B, fork3func func_C)
{
    pid_t A_pid, B_pid, C_pid = -1;

    A_pid = getpid();

    int first_fork_res = fork();
    if(first_fork_res == -1)
        syserr("fork");

    if(first_fork_res != 0)
    { // A
        B_pid = first_fork_res;

        assert(A_pid == getpid());
        return func_A(A_pid, B_pid, C_pid);
    }
    else
    { // B
        B_pid = getpid();
        
        int second_fork_res = fork();
        if(second_fork_res == -1)
            syserr("fork");

        if(second_fork_res != 0)
        { // B
            C_pid = second_fork_res;

            assert(getpid() == B_pid);
            assert(getppid() == A_pid);
            return func_B(A_pid, B_pid, C_pid);
        }
        else
        { // C
            C_pid = getpid();

            assert(getpid() == C_pid);
            assert(getppid() == B_pid);
            return func_C(A_pid, B_pid, C_pid);
        }
    }
}

static void syserr(const char *fmt, ...)
{
    va_list fmt_args;
    int err;

    fprintf(stderr, "ERROR: ");
    err = errno;

    va_start(fmt_args, fmt);
    if (vfprintf(stderr, fmt, fmt_args) < 0) {
        fprintf(stderr, " (also error in syserr) ");
    }
    va_end(fmt_args);
    fprintf(stderr, "(%d; %s)\n", err, strerror(err));
    exit(EXIT_FAILURE);
}


#endif