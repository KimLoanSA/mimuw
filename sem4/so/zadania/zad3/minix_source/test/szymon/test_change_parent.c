#include <unistd.h>
#include <stdio.h>

int main(int argc, char** argv)
{
        printf("moj pid %d | pid parenta %d\n", getpid(), getppid());
        printf("change parent... %d\n", changeparent());
        printf("moj pid %d | pid parenta %d | orginal parent %d\n", getpid(), getppid(), getoppid(getpid()));
	
        //printf("%d %d %d\n", getoppid(getpid()), getppid(), getpid());
}