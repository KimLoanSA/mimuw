#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define REQUIRE(cond, expr) do { if(!(cond)) { expr; } } while (0)
#define SYSERR(...) syserr(__FILE__,  __LINE__, __VA_ARGS__)
#define FATAL(...) fatal(__FILE__, __LINE__, __VA_ARGS__)


char *DEVICE;
char *MNTPT;

void areyousure() {
	char c;
	scanf(" %c", &c);
	if(c != 'y') {
		exit(1);
	}
}

int mountdev() {
	return minix_mount(DEVICE, MNTPT, 0, 0, "mfs", "");
}

int unmountdev() {
	return minix_umount(MNTPT, 0);
//	char cmd[1024];
//	snprintf(cmd, 1024, "umount %s", MNTPT);
//	return system(cmd);
}


void fatal(char *file, int line, const char *fmt, ...) {
  va_list fmt_args;

  fprintf(stderr, "%s:%d - ", file, line);
  fprintf(stderr, "ERROR: ");

  va_start(fmt_args, fmt);
  if (vfprintf(stderr, fmt, fmt_args) < 0) {
    fprintf(stderr, " (also error in fatal) ");
  }
  va_end(fmt_args);

  fprintf(stderr, "\n");
  chdir("/");
  unmountdev();
  exit(1);
}

void syserr(char *file, int line, const char *fmt, ...) {
  va_list fmt_args;
  int err;

  fprintf(stderr, "%s:%d - ", file, line);
  fprintf(stderr, "ERROR: ");
  err = errno;

  va_start(fmt_args, fmt);
  if (vfprintf(stderr, fmt, fmt_args) < 0) {
    fprintf(stderr, " (also error in syserr) ");
  }
  va_end(fmt_args);
  fprintf(stderr, " (%d; %s)\n", err, strerror(err));
  chdir("/");
  unmountdev();
  exit(1);
}

int exists(char *name) {
	return access(name, F_OK) == 0;
}


void basictestA() {
	int r;
	char buf[1024];
	char all[256];
	memset(buf, 0, 1024);
	for(int i = 0; i < 256; i++) all[i] = i;

	int KEY = open("KEY", O_CREAT | O_RDWR, 0777);
	REQUIRE(KEY >= 0, SYSERR("Error opening/creating KEY"));

	int f = open("file", O_CREAT | O_RDWR, 0777);
	REQUIRE(f >= 0, SYSERR("Error opening/creating file"));
	REQUIRE(close(f) == 0, SYSERR("Closing data file"));


	printf("Test writing to key\n");
	REQUIRE(write(KEY, buf, 10) == -1 && errno == EINVAL, SYSERR("write 10 bytes to KEY (success or invalid error code)"));
	REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));
	printf("OK\n");

	printf("Test reading from key\n");
	REQUIRE(read(KEY, buf, 10) == -1 && errno == EPERM, SYSERR("KEY can be read (or invalid error code)"));
	REQUIRE(read(KEY, buf, 1) == -1 && errno == EPERM, SYSERR("KEY can be read (or invalid error code)"));
	printf("OK\n");

	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));

	printf("Writing with O_APPEND\n");
	KEY = open("KEY", O_APPEND | O_RDWR);
	REQUIRE(KEY >= 0, SYSERR("Error opening file"));

	REQUIRE(write(KEY, buf, 10) == -1 && errno == EINVAL, SYSERR("write 10 bytes to KEY (success or invalid error code)"));
	REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));
	printf("OK\n");

	// All bytes for all keys, decrypted == original
	printf("Testing decrypted == original\n");
	for(int i = 0; i < 256; i++) {
		buf[0] = i;
		REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));

		f = open("file",  O_RDWR | O_TRUNC, 0777);
		REQUIRE(f >= 0, SYSERR("Error opening/creating file"));

		REQUIRE(write(f, all, 256) == 256, SYSERR("Error writing to data file"));
		REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek to file begin"));
		REQUIRE(read(f, buf, 256) == 256, SYSERR("Reading from data file"));
		for(int j = 0; j < 256; j++) {
			REQUIRE(all[j] == buf[j], FATAL("Invalid encryption"));
		}

		REQUIRE(close(f) == 0, SYSERR("Closing data file"));
	}
	printf("OK\n");

	printf("Test encryption correct\n");
	for(int i = 0; i < 256; i++) {
		buf[0] = i;
		REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));

		f = open("file",  O_RDWR | O_TRUNC, 0777);
		REQUIRE(f >= 0, SYSERR("Error opening/creating file"));

		REQUIRE(write(f, all, 256) == 256, SYSERR("Error writing to data file"));
		REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek to file begin"));

		buf[0] = 0;
		REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));

		REQUIRE(read(f, buf, 256) == 256, SYSERR("Reading from data file"));
		for(int j = 0; j < 256; j++) {
			REQUIRE((char)(all[j] + i) == buf[j], FATAL("Invalid encryption"));
		}

		REQUIRE(close(f) == 0, SYSERR("Closing data file"));

	}
	printf("OK\n");

	printf("Test decryption corrent\n");
	for(int i = 0; i < 256; i++) {
		buf[0] = 0;
		REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));

		f = open("file",  O_RDWR | O_TRUNC, 0777);
		REQUIRE(f >= 0, SYSERR("Error opening/creating file"));

		REQUIRE(write(f, all, 256) == 256, SYSERR("Error writing to data file"));
		REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek to file begin"));

		buf[0] = i;
		REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));

		REQUIRE(read(f, buf, 256) == 256, SYSERR("Reading from data file"));
		for(int j = 0; j < 256; j++) {
			REQUIRE((char)(all[j] - i) == buf[j], FATAL("Invalid encryption"));
		}

		REQUIRE(close(f) == 0, SYSERR("Closing data file"));

	}
	printf("OK\n");

	printf("Cleaning up\n");
	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));

	REQUIRE(exists("KEY"), FATAL("KEY doesn't exist"));
	REQUIRE(exists("file"), FATAL("file doesn't exist"));

	REQUIRE(unlink("KEY") == 0, SYSERR("Deleting KEY"));
	REQUIRE(unlink("file") == 0, SYSERR("Deleting file"));

	REQUIRE(!exists("KEY"), FATAL("KEY exists after deletion"));
	REQUIRE(!exists("file"), FATAL("file exists after deletion"));

	printf("OK\n");
}


void basictestB() {
	int r;
	char buf[1024];
	char all[256];
	memset(buf, 0, 1024);
	for(int i = 0; i < 256; i++) all[i] = i;

	printf("Remounting\n");
	REQUIRE(chdir("/") == 0, SYSERR("Changing location"));
	REQUIRE(unmountdev() == 0, SYSERR("Unmounting device"));
	REQUIRE(mountdev() == 0, SYSERR("Unmounting device"));
	REQUIRE(chdir(MNTPT) == 0, SYSERR("Changing location"));
	printf("OK\n");

	printf("Check RW is blocked\n");
	int f = open("file", O_CREAT | O_RDWR, 0777);
	REQUIRE(f >= 0, SYSERR("Error opening/creating file"));

	REQUIRE((r = write(f, buf, 1)) == -1 && errno == EPERM, SYSERR("Can write while blocked (or wrong error code) ret: %d", r));
	REQUIRE((r = write(f, buf, 10)) == -1 && errno == EPERM, SYSERR("Can write while blocked (or wrong error code) ret: %d", r));

	REQUIRE((r = read(f, buf, 1)) == -1 && errno == EPERM, SYSERR("Can read while blocked (or wrong error code) ret: %d", r));
	REQUIRE((r = read(f, buf, 10)) == -1 && errno == EPERM, SYSERR("Can read while blocked (or wrong error code) ret: %d", r));
	printf("OK\n");

	printf("Set key\n");
	int KEY = open("KEY", O_CREAT | O_RDWR, 0777);
	REQUIRE(KEY >= 0, SYSERR("Error opening/creating KEY"));
	REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY can't be set"));
	printf("OK\n");

	printf("Check RW is unblocked\n");
	REQUIRE((r = write(f, buf, 1)) == 1, SYSERR("Can write while blocked (or wrong error code) ret: %d", r));
	REQUIRE((r = write(f, buf, 10)) == 10, SYSERR("Can write while blocked (or wrong error code) ret: %d", r));

	REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek to file begin"));

	REQUIRE((r = read(f, buf, 1)) == 1, SYSERR("Can read while blocked (or wrong error code) ret: %d", r));
	REQUIRE((r = read(f, buf, 10)) == 10, SYSERR("Can read while blocked (or wrong error code) ret: %d", r));
	printf("OK\n");

	printf("Cleaning up\n");

	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));
	REQUIRE(close(f) == 0, SYSERR("Closing data file"));

	REQUIRE(exists("KEY"), FATAL("KEY doesn't exist"));
	REQUIRE(exists("file"), FATAL("file doesn't exist"));

	REQUIRE(unlink("KEY") == 0, SYSERR("Deleting KEY"));
	REQUIRE(unlink("file") == 0, SYSERR("Deleting file"));

	REQUIRE(!exists("KEY"), FATAL("KEY exists after deletion"));
	REQUIRE(!exists("file"), FATAL("file exists after deletion"));

	printf("OK\n");
}

void checkC(int dir) {
	int r;
	char buf[1024];
	char all[256];
	memset(buf, 0, 1024);
	for(int i = 0; i < 256; i++) all[i] = i;

	printf("Check read/write without encryption\n");
	int f = open("file", O_CREAT | O_RDWR, 0777);
	REQUIRE(f >= 0, SYSERR("Error opening/creating file"));

	REQUIRE(write(f, all, 256) == 256, SYSERR("Error writing to data file"));
	REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek to file begin"));
	REQUIRE(read(f, buf, 256) == 256, SYSERR("Reading from data file"));
	REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek to file begin"));
	for(int j = 0; j < 256; j++) {
		REQUIRE(all[j] == buf[j], FATAL("Invalid encryption"));
	}
	printf("OK\n");

	printf("Test writing to KEY\n");
	int KEY = open("KEY", O_CREAT | O_RDWR, 0777);
	REQUIRE(KEY >= 0, SYSERR("Error opening/creating KEY"));
	REQUIRE(write(KEY, buf, 1) == -1 && errno == EPERM, SYSERR("KEY cant be set (or wrong error code)"));
	printf("OK\n");

	printf("Deleting NOT_ENCRYPTED\n");
	if(dir) {
		REQUIRE(rmdir("NOT_ENCRYPTED") == 0, SYSERR("Deleting NOT_ENCRYPTED"));
	} else {
		REQUIRE(unlink("NOT_ENCRYPTED") == 0, SYSERR("Deleting NOT_ENCRYPTED"));
	}

	printf("OK\n");


	printf("Set key\n");
	buf[0] = 42;
	REQUIRE(write(KEY, buf, 1) == 1 , SYSERR("KEY can't be set"));
	printf("OK\n");

	printf("Check file is encrypted\n");
	REQUIRE(read(f, buf, 256) == 256, SYSERR("Reading from data file"));
	for(int j = 0; j < 256; j++) {
		REQUIRE(all[j] == (char)(buf[j] + 42), FATAL("Invalid encryption idx %d", j));
	}
	printf("OK\n");

	printf("Cleaning up\n");

	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));
	REQUIRE(close(f) == 0, SYSERR("Closing data file"));

	REQUIRE(exists("KEY"), FATAL("KEY doesn't exist"));
	REQUIRE(exists("file"), FATAL("file doesn't exist"));

	REQUIRE(unlink("KEY") == 0, SYSERR("Deleting KEY"));
	REQUIRE(unlink("file") == 0, SYSERR("Deleting file"));

	REQUIRE(!exists("KEY"), FATAL("KEY exists after deletion"));
	REQUIRE(!exists("file"), FATAL("file exists after deletion"));

	printf("OK\n");

}

void basictestC() {
	printf("Create file NOT_ENCRYPTED\n");
	int NOT = open("NOT_ENCRYPTED", O_CREAT | O_RDWR, 0777);
	REQUIRE(NOT >= 0, SYSERR("Error opening/creating file"));
	REQUIRE(close(NOT) == 0, SYSERR("Closing data file"));
	printf("OK\n");

	checkC(0);


	printf("Create NOT_ENCRYPTED directory\n");
	REQUIRE(mkdir("NOT_ENCRYPTED", 0700) == 0, SYSERR("Error creating NOT_ENCRYPTED dir"));
	printf("OK\n");

	checkC(1);
}

void betweenBlocks() {
	printf("Creating file\n");
	int f = open("file", O_CREAT | O_RDWR, 0777);
	REQUIRE(f >= 0, SYSERR("Error opening/creating KEY"));
	char buf[5000];
	memset(buf, 0, 5000);
	REQUIRE(write(f, buf, 5000) == 5000, SYSERR("Writing to file"));
	REQUIRE(close(f) == 0, SYSERR("Closing data file"));
	REQUIRE(rename("file", "KEY") == 0, SYSERR("Renaming file"));
	printf("OK\n");


	printf("Trying to write 2 bytes to key\n");
	int KEY = open("KEY", O_RDWR, 0777);
	REQUIRE(KEY >= 0, SYSERR("Error opening KEY"));

	for(int i = 1020; i < 1030; i++) {
		REQUIRE(lseek(KEY, i, SEEK_SET) == i, SYSERR("Seek KEY pos %d", i));
		REQUIRE(write(KEY, buf, 2) == -1 && errno == EINVAL, FATAL("Written 2 bytes to KEY"));
		REQUIRE(write(KEY, buf, 1) == 1, SYSERR("Can't write to KEY"));
	}
	printf("OK\n");

	printf("Cleanup\n");
	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));
	REQUIRE(exists("KEY"), FATAL("KEY doesn't exist"));
	REQUIRE(unlink("KEY") == 0, SYSERR("Deleting KEY"));
	REQUIRE(!exists("KEY"), FATAL("KEY exists after deletion"));
	printf("OK\n");
}

void testrenameKEY() {
	int r;
	char buf[1024];
	char all[256];
	memset(buf, 0, 1024);
	for(int i = 0; i < 256; i++) all[i] = i;

	printf("Write to KEY\n");
	int KEY = open("KEY", O_CREAT | O_RDWR, 0777);
	REQUIRE(KEY >= 0, SYSERR("Error opening/creating KEY"));
	REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY cant be set (or wrong error code)"));
	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));
	printf("OK\n");

	printf("Rename KEY file\n");
	REQUIRE(rename("KEY", "file") == 0, SYSERR("Can't rename KEY to file"));
	printf("OK\n");

	printf("RW to file\n");
	int f = open("file", O_RDWR, 0777);
	REQUIRE(f >= 0, SYSERR("Error opening file"));
	REQUIRE(write(f, all, 256) == 256, SYSERR("Can't write to file"));
	REQUIRE(lseek(f, 0, SEEK_SET) == 0, SYSERR("Seek file pos 0"));
	REQUIRE(read(f, buf, 256) == 256, SYSERR("Can't read file"));
	for(int i = 0; i < 256; i++) REQUIRE(all[i] == buf[i], FATAL("Invalid RW"));
	REQUIRE(close(f) == 0, SYSERR("Close file"));
	printf("OK\n");

	printf("Rename file to KEY\n");
	REQUIRE(rename("file", "KEY") == 0, SYSERR("Can't rename KEY to file"));
	printf("OK\n");

	printf("Write to KEY\n");
	KEY = open("KEY", O_CREAT | O_RDWR, 0777);
	REQUIRE(KEY >= 0, SYSERR("Error opening/creating KEY"));
	REQUIRE(write(KEY, buf, 1) == 1, SYSERR("KEY cant be set (or wrong error code)"));
	REQUIRE(close(KEY) == 0, SYSERR("Close KEY"));
	printf("OK\n");

	printf("Cleanup\n");
	REQUIRE(exists("KEY"), FATAL("KEY doesn't exist"));
	REQUIRE(unlink("KEY") == 0, SYSERR("Deleting KEY"));
	REQUIRE(!exists("KEY"), FATAL("KEY exists after deletion"));
	printf("OK\n");
}


int main(int argc, char *argv[]) {
	int r;

	if(argc != 3) {
		printf("Usage: %s device-path mount-point\n", argv[0]);
		exit(0);
	}

	DEVICE = argv[1];
	MNTPT = argv[2];

	printf("\n");
	printf("======================================================\n");
	printf("=====!This will erase all data on supplied drive!=====\n");
	printf("===============Do you want to continue?===============\n");
	printf("========================y/[n]=========================\n");
	printf("======================================================\n");
	areyousure();

	char cmd[1024];
	snprintf(cmd, 1024, "/sbin/mkfs.mfs %s", argv[1]);
	printf("Recreating file system on device %s\n", argv[1]);
	printf("Command: %s\n", cmd);
	printf("Do you want to continue? y/[n]\n");
	areyousure();

	if((r = system(cmd))) {
		FATAL("Error while formatting drive! Code: %d", r);
	}

	printf("Mounting device %s in location %s\n", argv[1], argv[2]);
	if((r = mountdev())) {
		SYSERR("Error while mounting drive! Code: %d", r);
	}

	printf("Changing directory to %s\n", MNTPT);
	if((r = chdir(MNTPT))) {
		SYSERR("Can't change directory to mountpoint!");
	}

	printf("==============TEST A=================\n");
	basictestA();
	printf("==============TEST B=================\n");
	basictestB();
	printf("==============TEST C=================\n");
	basictestC();
	printf("==============TEST write to key between blocks=================\n");
	betweenBlocks();
	printf("==============TEST KEY rename========\n");
	testrenameKEY();

	printf("==============Cleanup=================\n");
	printf("Unmount drive\n");
	if((r = chdir("/"))) {
		SYSERR("Can't change directory!");
	}
	REQUIRE(unmountdev() == 0, SYSERR("Unmounting after tests"));
	printf("OK\n");
}
