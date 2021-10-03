#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>

// ICMP header
#include <netinet/ip_icmp.h>
#include "err.h"

#define BSIZE 1000
#define ICMP_HEADER_LEN 8
#define IP_HEADER_LEN 20

unsigned short in_cksum(unsigned short *addr, int len);
void drop_to_nobody();

void send_spoof_ping_request(int sock, char* s_dst_addr, char* s_spoof_addr) {
  struct sockaddr_in dst_addr;
  struct sockaddr_in spoof_addr;

  struct icmp* icmp;
  struct iphdr* ip;
  
  char send_buffer[BSIZE];
  
  ssize_t data_len = 0;
  ssize_t icmp_len = 0;
  ssize_t len = 0;

  memset(&dst_addr, 0, sizeof(dst_addr));
  if (inet_pton(AF_INET, s_dst_addr, (struct in_addr *) &dst_addr.sin_addr.s_addr) != 1)
    syserr("inet_pton (dst)");
  dst_addr.sin_family = AF_INET;
  dst_addr.sin_port = 0;

  memset(&spoof_addr, 0, sizeof(dst_addr));
  if (inet_pton(AF_INET, s_spoof_addr, (struct in_addr *) &spoof_addr.sin_addr.s_addr) != 1)
    syserr("inet_pton (spoof)");
  spoof_addr.sin_family = AF_INET;
  spoof_addr.sin_port = 0;
         
  memset(send_buffer, 0, sizeof(send_buffer));
  ip = (struct iphdr *) send_buffer;
  ip->ihl = IP_HEADER_LEN >> 2; // ihl is in 32-bit words
  ip->version = 4; // IPv4
  ip->tos = 0; // no type of service
  ip->frag_off = 0; // no fragmentation
  ip->ttl = 64; // standard TTL
  ip->protocol = IPPROTO_ICMP; // carries ICMP
  ip->check = 0; // will be filled up by the system
  ip->saddr = spoof_addr.sin_addr.s_addr;
  ip->daddr = dst_addr.sin_addr.s_addr;

  // initializing ICMP header
  icmp = (struct icmp *) (send_buffer + IP_HEADER_LEN);
  icmp->icmp_type = ICMP_ECHO;
  icmp->icmp_code = 0;
  icmp->icmp_id = htons(getpid()); // process identified by PID
  icmp->icmp_seq = htons(0); // sequential number
  data_len = snprintf(((char*) send_buffer+IP_HEADER_LEN+ICMP_HEADER_LEN), sizeof(send_buffer)-ICMP_HEADER_LEN-IP_HEADER_LEN, "BASIC PING!");
  if (data_len < 1)
    syserr("snprinf");
  icmp_len = data_len + ICMP_HEADER_LEN; // packet is filled with 0
  icmp->icmp_cksum = 0; // checksum computed over whole ICMP package
  icmp->icmp_cksum = in_cksum((unsigned short*) icmp, icmp_len);

  ip->tot_len = htons(IP_HEADER_LEN + ICMP_HEADER_LEN + data_len);

  len = sendto(sock, (void*) send_buffer, ntohs(ip->tot_len), 0, (struct sockaddr *) &dst_addr, 
               (socklen_t) sizeof(dst_addr));
  if (ntohs(ip->tot_len) != (ssize_t) len)
    syserr("partial / failed write");

  printf("wrote %zd bytes\n", len);

}


int main(int argc, char *argv[]) {
  int sock;
  
  if (argc < 3) {
    fatal("Usage: %s dst_host spoof_src_host\n", argv[0]);
  }

  // RAW socket with IPPROTO_RAW (we will set the IP header)
  sock = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
  if (sock < 0)
    syserr("socket");
  
  drop_to_nobody();

  send_spoof_ping_request(sock, argv[1], argv[2]);

  if (close(sock) == -1) 
    syserr("close"); 

  return 0;
}


