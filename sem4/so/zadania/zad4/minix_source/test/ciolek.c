#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <assert.h>
#include <sys/wait.h>

// A creates children
// B - kudos = 0
// C - kudos = 10
// D - kudos = 25
// E - kudos = 50

// All of them do work and print results


static int do_one_job(int previous_result)
{
  int result = previous_result;
  for(int i = 0; i < 10000; i++)
  {
    if(((i + 100 * result) % 89 + 878) % 107 == 34)
      result += (i * 34) + (i / 23 + i % 1069);
  }

  if(result % 10203201 == 1020301)
    result = 23221232;

  return result;
}

void do_work(const char* worker_name)
{
  static const int work_time_secs = 10;

  unsigned long long work_start_time = time(NULL);

  int jobs_done = 0;
  int job_res = 0;

  while(time(NULL) - work_start_time < work_time_secs)
  {
    job_res = do_one_job(job_res);
    jobs_done++;
  }

  if(job_res == 2323432)
    printf("That will not happen for sure");

  printf("Worker %s did %d jobs\n", worker_name, jobs_done); fflush(stdout);
}

void do_worker(const char* worker_name)
{
  sleep(1);
  printf("Worker %s starts working...\n", worker_name); fflush(stdout);
  do_work(worker_name);
}

int give_many_kudos(pid_t pid, int kudos)
{
  int result = -2;
  for(int i = 0; i < kudos; i++)
    result = givekudos(pid);

  return result;
}

void give_the_kudos(pid_t c_pid, pid_t d_pid, pid_t e_pid)
{
  assert(give_many_kudos(c_pid, 10) == 2);
  assert(give_many_kudos(d_pid, 25) == 1);
  assert(give_many_kudos(e_pid, 50) == 0);
}

int main()
{
  pid_t a_pid = getpid();

  pid_t b_pid = fork();
  assert(b_pid != -1);

  if(b_pid == 0)
  {
    do_worker("B");
    return 0;
  }

  pid_t c_pid = fork();
  assert(c_pid != -1);

  if(c_pid == 0)
  {
    do_worker("C");
    return 0;
  }

  pid_t d_pid = fork();
  assert(d_pid != -1);

  if(d_pid == 0)
  {
    do_worker("D");
    return 0;
  }

  pid_t e_pid = fork();
  assert(e_pid != -1);

  if(e_pid == 0)
  {
    do_worker("E");
    return 0;
  }

  pid_t give_pid = fork();
  assert(give_pid != -1);

  if(give_pid == 0)
  {
    give_the_kudos(c_pid, d_pid, e_pid);
    return 0;
  }

  for(int i = 0; i < 5; i++) // Wait for B C D E <givekudos>
    wait(NULL);
}