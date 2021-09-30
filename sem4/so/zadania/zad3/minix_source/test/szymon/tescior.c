#include <unistd.h>
#include <stdio.h>

int main(int argc, char** argv)
{
        printf("%d\n", changeparent());
        printf("%d %d %d\n", getoppid(getpid()), getppid(), getpid());
}