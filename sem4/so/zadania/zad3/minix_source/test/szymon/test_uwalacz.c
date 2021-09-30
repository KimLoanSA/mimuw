#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h> 
#include <sys/wait.h> 

int main(int argc, char** argv)
{
	int xd;
	assert((xd =getoppid(72)) == -1);
	printf("%d %d\n", xd, errno);
	assert(errno == EINVAL);
	return 0;
}
