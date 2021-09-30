#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h> 
#include <sys/wait.h> 

int main(int argc, char** argv)
{
    int pidA = getpid();
    int res =givekudos(pidA);
    printf(" rezultat: %d, errno: %d\n", res, errno);
    assert(errno == EPERM);

    return 0;
}
