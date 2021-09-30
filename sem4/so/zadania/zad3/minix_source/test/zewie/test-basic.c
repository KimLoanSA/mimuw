#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/types.h>
#include <fcntl.h>

#define ullong unsigned long long

int count = 1;
int nr = 0;
int counter = 0;

pid_t me = 0;

#define FMT "%02d:%03d "

char buffer[1000];
int len;
int output;

#define printf(...) \
do { \
	len = snprintf(buffer, 100, __VA_ARGS__); \
	write(output, buffer, len); \
} while(0)

void deb() {
	printf("%02d;%03d pid: P%dID, ppid: P%dID, oppid: P%dID\n", counter++, nr, me, getppid(), getoppid(me));
	if (me != getpid()) {
		printf("FATAL pid %d %d\n", me, getpid());
		exit(1);
	}
}


void msleep(ullong a) {
	usleep(a * 1000);
}

ullong mtime() {
	struct timeval t;
	gettimeofday(&t, 0);
	return t.tv_sec * 1000 + t.tv_usec / 1000;
}

ullong lastRand = 1;

unsigned int rander() {
	lastRand += (lastRand == 0);
	lastRand = lastRand * 1231312 % 717231 + 1263172;
	return lastRand / 6767;
}

int main() {
	int status;
	ullong start = mtime();
	int seed;
	scanf("%d", &seed);
	lastRand = seed;
	me = getpid();
	
	close(0);
	close(1);
	output = open("output.txt", O_WRONLY | O_CREAT);

	if (output < 0) {
		fprintf(stderr, "nie ma pliku\n");
		exit(1);
	}
	
	pid_t pid;
	
	/* robienie ścieżki */
	for (int i = 1; i <= 64; i++) {
		int jestemDzieciem = 0;
		switch(pid = fork()) {
			case -1:
				printf("FATAL FORK\n");
				exit(1);
				break;
			case 0:
				me = getpid();
				nr = i;
				printf(FMT "jestem nowy P%dID\n", counter++, nr, getpid());
				deb();
				jestemDzieciem = 1;
				break;
			default:
				printf(FMT "mam nowe dziecko P%dID\n", counter, nr, pid);
				break;
		}
		if (jestemDzieciem) {
			break;
		}
	}
	
	/* robienie drzewa */
	counter = 5;
	if (nr == 64) {
		for (int i = 0; i < 6; i++) {
			fflush(stdout);
			switch(pid = fork()) {
				case -1:
					printf("FATAL FORK\n");
					exit(1);
					break;
				case 0:
					me = getpid();
					nr = nr + count;
					printf(FMT "jestem nowy P%dID\n", counter++, nr, getpid());
					deb();
					break;
				default:
					printf(FMT "mam nowe dziecko P%dID\n", counter++, nr, pid);
					deb();
					break;
			}
			if (nr == 0) {
				printf(FMT "\n", counter, nr);
			}
			count *= 2;
		}
	}
	
	/* mapowanie pid na nr */
	counter = 25;
	if (nr == 0) {
		printf("MAPA P%dID BŁĄD\n", -1);
		printf("MAPA P%dID PUSTO\n", 0);
		printf("MAPA P%dID INIT\n", 1);
		printf("MAPA P%dID SHELL\n", getppid());
	}
	printf("MAPA P%dID %d\n", me, nr);
	
	
	
	/* desync rand */
	for (int i = 0; i < nr * 20; i++) {
		rander();
	}
	
	if (nr == 0) {
		/* arcyprzodek czeka na chętnych */
		int found = 0;
		do {
			found = 0;
			errno = 0;
			pid_t res = wait(&status);
			counter = WEXITSTATUS(status);
			printf(FMT "(arcy)zebrałem dziecko: P%dID errno: %d\n", counter, nr, res, errno);
			deb();
			if (res < 0) {
				if (errno == ECHILD) {
					found = 0;
				} else {
					printf("FATAL wait %d\n", errno);
					exit(1);
				}
			} else {
				found = 1;
			}
			
		} while(found);
		/* i kończy */
		return 0;
	}
	
	int res;
	for (ullong i = 1; i < 200; i++) {
		ullong kolej = (i * 130 + nr) * 50;
		while (mtime() - start < kolej) {
			msleep(1);
		}
		ullong poczatek = (mtime() - start) - kolej;

		int typ = (rander() + nr) % 8;
		errno = 0;
		switch(typ) {
			case 0:;
			case 1:;
			case 2:;
			case 3:;
				do {
					errno = 0;
					res = waitpid(-1, &status, WNOHANG);
					printf(FMT "czekałem szybko: P%dID errno: %d\n", counter, nr, res, errno);
					if (res < 0 && errno != ECHILD && errno != EAGAIN) {
						printf(FMT "FATAL wait na dzieci szybko %d %d\n", counter, nr, res, errno);
					}
				} while (res > 0);
				deb();
				break;
			case 4:;
			case 5:;
				res = changeparent();
				printf(FMT "zmieniam ojca: %d errno: %d\n", counter, nr, res, errno);
				deb();
				break;
			case 6:;
				printf(FMT "czekam na śmierć\n", counter, nr);
				deb();
				res = 1;
				while (res > 0) {
					errno = 0;
					res = wait(&status);
					printf(FMT "zebrałem dziecko: P%dID errno: %d\n", counter, nr, res, errno);
					if (res < 0 && errno != ECHILD) {
						printf(FMT "FATAL wait na dzieci %d %d\n", counter, nr, res, errno);
					}
				}
				printf(FMT "dzieci poszły jestem wolny\n", counter++, nr);
				exit(counter);
				break;
			default:;
				/* desync change and wait */
				printf(FMT "a ja kończę\n", counter, nr);
				deb();
				exit(counter);
				break;
		}

		ullong roznica = (mtime() - start) - kolej;
		if (roznica > 30) fprintf(stderr, FMT "Czas %llu,%llu,%llu niebezpiecznie duży\n", counter, nr, kolej, poczatek, roznica);
	}
	close(output);
}
