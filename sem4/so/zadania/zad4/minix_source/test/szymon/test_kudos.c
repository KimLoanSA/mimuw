#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h> 
#include <sys/wait.h> 
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
        pid_t pidek = atoi(argv[1]);
	//zakladamy ze proces z pidek nie jest z nami zwiazany
	printf("Expected (errno, givekudos) == (0,3)\n");
	for(int i = 1;i<10;++i) {
		assert(givekudos(pidek)==3);
		assert(errno==0);
	}

	printf("Expected (errno, givekudos) == (0,2)\n");
	for(int i = 1;i<15;++i) {
		int x = givekudos(pidek);
		printf("got givekudos == %d\n", x);
		assert(x==2);
		assert(errno==0);
	}

	/*for(int i = 1;i<25;++i) {
		assert(givekudos(pidek)==1);
		assert(errno==0);
	}

	for(int i = 1;i<20;++i) {
		assert(givekudos(pidek)==0);
		assert(errno==0);
	}*/

        return 0;
}
