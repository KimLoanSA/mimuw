#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <netinet/ip_icmp.h>

#include "err.h"

#define BSIZE 1000
#define ICMP_HEADER_LEN 8

unsigned short in_cksum(unsigned short *addr, int len);
void drop_to_nobody();
void catch_sigint(int sig);
void set_interrupt_signal_handler();
long long get_current_time();

static int is_program_interrupted = 0;
static int icmp_seq_send = 0;

void send_ping_request(int sock, char *s_send_addr) {
  struct addrinfo addr_hints;
  struct addrinfo *addr_result;
  struct sockaddr_in send_addr;

  struct icmp *icmp;

  char send_buffer[BSIZE];

  int err = 0;
  ssize_t data_len = 0;
  ssize_t icmp_len = 0;
  ssize_t len = 0;

  memset(&addr_hints, 0, sizeof(struct addrinfo));
  addr_hints.ai_family = AF_INET;
  addr_hints.ai_socktype = SOCK_RAW;
  addr_hints.ai_protocol = IPPROTO_ICMP;
  err = getaddrinfo(s_send_addr, 0, &addr_hints, &addr_result);
  if (err != 0) {
    syserr("getaddrinfo: %s\n", gai_strerror(err));
  }

  send_addr.sin_family = AF_INET;
  send_addr.sin_addr.s_addr = ((struct sockaddr_in *) (addr_result->ai_addr))->sin_addr.s_addr;
  send_addr.sin_port = htons(0);
  freeaddrinfo(addr_result);

  memset(send_buffer, 0, sizeof(send_buffer));
  icmp = (struct icmp *) send_buffer;
  icmp->icmp_type = ICMP_ECHO;
  icmp->icmp_code = 0;
  icmp->icmp_id = htons(getpid());
  icmp->icmp_seq = htons(icmp_seq_send++);

  data_len = snprintf(((char*) send_buffer+ICMP_HEADER_LEN), sizeof(send_buffer)-ICMP_HEADER_LEN, "%lld", get_current_time());
  if (data_len < 1) {
    syserr("snprintf");
  }

  icmp_len = data_len + ICMP_HEADER_LEN;
  icmp->icmp_cksum = 0;
  icmp->icmp_cksum = in_cksum((unsigned short *) icmp, icmp_len);

  len = sendto(sock, (void *) icmp, icmp_len, 0, (struct sockaddr *) &send_addr, (socklen_t) sizeof(send_addr));
  if (icmp_len != (ssize_t) len) {
    syserr("partial / failed write");
  }
}

int receive_ping_reply(int sock) {
  struct sockaddr_in rcv_addr;
  socklen_t rcv_addr_len;

  struct ip *ip;
  struct icmp *icmp;

  char rcv_buffer[BSIZE];

  ssize_t ip_header_len = 0;
  ssize_t time_index = 0;
  ssize_t icmp_len = 0;
  ssize_t len;
  long long sending_package_time;

  memset(rcv_buffer, 0, sizeof(rcv_buffer));
  rcv_addr_len = (socklen_t) sizeof(rcv_addr);
  len = recvfrom(sock, (void *) rcv_buffer, sizeof(rcv_buffer), 0, (struct sockaddr *) &rcv_addr, &rcv_addr_len);

  if (len == -1) {
    syserr("failed read");
  }

  ip = (struct ip *) rcv_buffer;
  ip_header_len = ip->ip_hl << 2;

  icmp = (struct icmp *) (rcv_buffer + ip_header_len);
  icmp_len = len - ip_header_len;

  printf("%zd bytes from %s ", len, inet_ntoa(rcv_addr.sin_addr));
  printf("icmp_seq=%d ttl=%d ", ntohs(icmp->icmp_seq), ip->ip_ttl);

  if (icmp_len < ICMP_HEADER_LEN) {
    fatal("icmp header len (%d) < ICMP_HEADER_LEN", icmp_len);
  }

  if (icmp->icmp_type != ICMP_ECHOREPLY) {
    printf("strange reply type (%d)\n", icmp->icmp_type);
    return 0;
  }

  if (ntohs(icmp->icmp_id) != getpid()){
    fatal("reply with id %d different from my pid %d", ntohs(icmp->icmp_id), getpid());
  }

  time_index = ip_header_len + ICMP_HEADER_LEN;
  sending_package_time = strtoll(rcv_buffer + time_index, NULL, 10);
  double time = (double)(get_current_time() - sending_package_time) / (double) 1000;

  printf("time=%.3f ms\n", time);

  return 1;
}

int main(int argc, char *argv[]) {
  int sock;

  if (argc < 2) {
    fatal("Usage: %s host\n", argv[0]);
  }

  sock = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
  if (sock < 0) {
    syserr("socket");
  }

  set_interrupt_signal_handler();
  drop_to_nobody();

  while (is_program_interrupted == 0) {
    send_ping_request(sock, argv[1]);

    while (!receive_ping_reply(sock)) {
      // empty
    }

    sleep(1);
  }

  if (close(sock) == -1) {
    syserr("close");
  }

  return 0;
}

void set_interrupt_signal_handler() {
  sigset_t block_mask;
  struct sigaction action;

  sigemptyset (&block_mask);
  action.sa_handler = catch_sigint;
  action.sa_mask = block_mask;
  action.sa_flags = SA_RESTART;

  if (sigaction(SIGINT, &action, 0) != 0) {
    syserr("sigaction");
  }
}

void catch_sigint(int sig) {
  is_program_interrupted = 1;
}

long long get_current_time() {
  struct timeval timeval;
  gettimeofday(&timeval, 0);
  return timeval.tv_sec * 1000000 + timeval.tv_usec;
}