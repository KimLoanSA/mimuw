#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h> 
#include <sys/wait.h> 

int main(int argc, char** argv)
{
	pid_t pidA, pidB, pidC, pidD;
	pid_t ppidA, ppidB, ppidC, ppidD;
	pidA = getpid();
	ppidA = getppid();
	assert(getoppid(pidA) == ppidA);
	fork();
	if(getpid() != pidA){
		pidB = getpid();
		ppidB = getppid();
		assert(ppidB == getoppid(pidB));
		fork();
		if(getpid() != pidB){
			pidC = getpid();
			assert(changeparent() == 0);
			assert(getppid() == pidA);
			assert(getoppid(pidC) == pidB);
			return 0;
		} else {
			sleep(1);
			return 0;
		}
	} else {
		wait(NULL);
	}
	printf("Klasyk - GOOD\n");

	assert(getpid() == pidA);
	ppidA = getppid();
	fork();
        if(getpid() != pidA){
		ppidB = getppid();
		assert(pidA == ppidB);
                pidB = getpid();
                fork();
                if(getpid() != pidB){
			sleep(1); // sleep so that B wait()
                        pidC = getpid();
                        assert(changeparent() == -1); // EPERM
			assert(errno == EPERM);
                        assert(getppid() == pidB);
                        assert(getoppid(pidC) == pidB);
                        return 0;
                } else {
			assert(getppid() == ppidB);
                        wait(NULL);
			assert(changeparent() == 0);
			assert(getppid() == ppidA);
	 		assert(getoppid(pidB) == ppidB);
                        return 0;
                }
        }

	assert(getpid() == pidA);
	sleep(3);
	printf("EPERM, masowe czekanie - GOOD\n");

	fork();
        if(getpid() != pidA){
			printf("ppidA=%d, pidA = %d, dziecko = %d, ojciec dziecka%d\n",ppidA, pidA, getpid(), getppid());
               	ppidB = getppid();
               	assert(pidA == ppidB);
               	pidB = getpid();
               	assert(getppid() == ppidB);		
              	assert(changeparent() == 0);
				printf("pidA = %d, dziecko = %d, ojciec dziecka%d\n", pidA, getpid(), getppid());
            	assert(getppid() == ppidA);
             	assert(getoppid(pidB) == ppidB);
		
		if(getoppid(getppid()) == 1) {
			assert(changeparent() == -1);
			assert(errno == EACCES);
			assert(getppid() == ppidA);
			assert(getoppid(pidB) == ppidB);
		} else {
			errno = EINVAL;
			printf("[PRZED] ja =%d, moj tata=%d\n",getpid(), getppid());
			int xx = changeparent();
			printf("[PO] ja =%d, moj tata=%d\n",getpid(), getppid());
			printf("pidA =%d, ppidA=%d, xx=%d, errno=%d\n",pidA, ppidA, xx, errno);

			assert(xx == 0 || errno == EPERM);
                	if(errno == EACCES) {
				assert(getppid() == getoppid(ppidA));
                	} else {
				assert(getppid() == ppidA);
				assert(changeparent() == -1);
				assert(errno == EPERM);
			}
			assert(getoppid(pidB) == ppidB);
		}
              	return 0;
        }
	sleep(2);
	printf("EPERM, shell jest mocny - GOOD\n");

	assert(pidA == getpid());
	fork();
	if(getpid() != pidA){
		assert(getppid() == getoppid(getpid()));
		sleep(1);
		assert(getoppid(getpid()) == pidA); // ojciec juz padl
		int xd;
		assert((xd =getoppid(72)) == -1);
		printf("%d %d\n", xd, errno);
		assert(errno == EINVAL);
		assert(changeparent() == -1);
		assert(errno == EACCES);
		return 0;
	}

	printf("EINVAL i EACCES - GOOD\n");
	return 0;
}
