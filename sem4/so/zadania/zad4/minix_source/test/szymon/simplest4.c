#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/resource.h>


int main(int  argc, char* argv[]) {
	if (argc < 2) {
		puts("NOARG");
		return 1;
	}
	pid_t pidek = atoi(argv[1]);
	printf("%d %d %d\n", givekudos(pidek), errno, getpriority(PRIO_PROCESS, pidek));
	return 0;
}

