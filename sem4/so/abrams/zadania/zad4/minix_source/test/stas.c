#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

int main()
{
  /*for (int i=0; i<100; i++)
    printf("%d\n", select());
  return 0;*/

  /*int son1, parentpid = getpid(), start = time(0);
  if (!(son1 = fork())) {
    int ile=0;
    while (time(0) - start < 10) {
      ile++;
    }
    printf("Son 1: %d\n", ile);
  } else {
    if (!fork()) {
      for (int i=0; i<10; i++) {
        givekudos(son1);
      }
      int ile=0;
      while (time(0) - start < 10) {
        ile++;
      }
      printf("Son 2: %d\n", ile);
    } else {
      printf("Father");
      sleep(15);
    }
  }
  sleep(100000);*/


  /*for (int i=0; i<100; i++)
    printf("%d\n", i);
  return 0;*/



//  printf("Start sleep\n");
//  sleep(5);
//  printf("End sleep\n");
//  return 0;






  printf("%d\n", givekudos(getpid()));
  printf("%d %d\n", errno, EPERM);

  printf("%d\n", givekudos(2137));
  printf("%d %d\n", errno, EINVAL);
  printf("%d\n", givekudos(4));
  printf("%d %d\n", errno, EPERM);

  int son1, parentpid = getpid();
  if (!(son1 = fork())) {
    printf("Son1 start");
    int a;
    for (int i=0; i<220000000; i++) {
      //printf("TURN\n");
      if (i%10000000 == 0)
        printf("Turn %d\n", i);
    }
    printf("Son1 end");
    time_t start = time(0);
    int ile=0;
    printf("Start 1: %lld\n", start);
    while (time(0) - start < 10)
      ile++;
    printf("Stop 1: %lld, ile=%d\n", time(0), ile);
  } else {
    if (!fork()) {
      printf("Son pid: %d\n", son1);
      printf("%d\n", givekudos(parentpid));
      printf("%d %d\n", errno, EPERM);
      for (int i=0; i<10; i++) {
        //printf("Give no: %d, result: %d\n", i+1, givekudos(son1));
        //printf("%d\n", errno);
        givekudos(son1);
      }
      printf("Done, result: %d\n", givekudos(son1));
      time_t start = time(0);
      int ile=0;
      printf("Start 2: %lld\n", start);
      while (time(0) - start < 10)
        ile++;
      printf("Stop 2: %lld, ile=%d\n", time(0), ile);
      //printf("Done\n");
    } else {
      time_t start = time(0);
      int ile=0;
      printf("Father: %lld\n", start);
      while (time(0) - start < 20)
        ile++;
      printf("Father: %lld, ile=%d\n", time(0), ile);
    }
  }
  return 0;
}