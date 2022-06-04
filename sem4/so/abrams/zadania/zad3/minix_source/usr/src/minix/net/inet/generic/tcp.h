/*
tcp.h

Copyright 1995 Philip Homburg
*/

#ifndef TCP_H
#define TCP_H

#define TCP_MAX_DATAGRAM	8192

#ifndef TCP_MAX_SND_WND_SIZE
#define TCP_MAX_SND_WND_SIZE	(32*1024)
#endif

#ifndef TCP_MIN_RCV_WND_SIZE
#define TCP_MIN_RCV_WND_SIZE	(4*1024)
#endif

#ifndef TCP_MAX_RCV_WND_SIZE
#define TCP_MAX_RCV_WND_SIZE	(TCP_MIN_RCV_WND_SIZE + 28*1024)
#endif

#define TCP_DEF_TOS		0
#define TCP_DEF_TTL		5	/* hops/seconds */
#define TCP_DEF_TTL_NEXT	30	/* hops/seconds */

/* An established  TCP connection times out if no communication is possible
 * for TCP_DEF_RT_DEAD clock ticks
 */
#ifndef TCP_DEF_RT_DEAD
#define TCP_DEF_RT_DEAD		(20L*60*HZ)
#endif

#define TCP_DEF_RT_MAX_CONNECT	(5L*60*HZ) /* 5 minutes in ticks */
#define TCP_DEF_RT_MAX_LISTEN	(1L*60*HZ) /* 1 minute in ticks */
#define TCP_DEF_RT_MAX_CLOSING	(1L*60*HZ) /* 1 minute in ticks */

/* Minimum and maximum intervals for zero window probes. */
#define TCP_0WND_MIN		(HZ)
#define TCP_0WND_MAX		(5*60*HZ)

#define TCP_DEF_RTT		15	/* initial retransmission time in
					 * ticks
					 */
#define TCP_RTT_GRAN		5	/* minimal value of the rtt is
					 * TCP_RTT_GRAN * CLOCK_GRAN
					 */
#define TCP_RTT_MAX		(10*HZ)	/* The maximum retransmission interval
					 * is TCP_RTT_MAX ticks
					 */
#define TCP_RTT_SMOOTH		 16	/* weight is 15/16 */
#define TCP_DRTT_MULT		  4	/* weight of the deviation */
#define TCP_RTT_SCALE		256	/* Scaled values for more accuracy */

#ifndef TCP_DEF_KEEPALIVE
#define TCP_DEF_KEEPALIVE	(20L*60*HZ)	/* Keepalive interval */
#endif

#ifndef TCP_DEF_MSS
#define TCP_DEF_MSS		1400
#endif

#define TCP_MIN_PATH_MTU	 500
#define TCP_PMTU_INCR_IV	(1L*60*HZ)	/* 1 minute in ticks */
#define TCP_PMTU_EN_IV		(10L*60*HZ)	/* 10 minutes in ticks */
#define TCP_PMTU_INCR_FRAC	100		/* Add 1% each time */
#define TCP_PMTU_BLACKHOLE	(10*HZ)		/* Assume a PMTU blackhole
						 * after 10 seconds.
						 */

#define TCP_DEF_CONF		(NWTC_COPY | NWTC_LP_UNSET | NWTC_UNSET_RA | \
					NWTC_UNSET_RP)
#define TCP_DEF_OPT		(NWTO_NOFLAG)

#define TCP_DACK_RETRANS	3	/* # dup ACKs to start fast retrans. */

struct acc;

void tcp_prep ARGS(( void ));
void tcp_init ARGS(( void ));
int tcp_open ARGS(( int port, int srfd,
	get_userdata_t get_userdata, put_userdata_t put_userdata, 
	put_pkt_t put_pkt, select_res_t select_res ));
int tcp_read ARGS(( int fd, size_t count));
int tcp_write ARGS(( int fd, size_t count));
int tcp_ioctl ARGS(( int fd, ioreq_t req));
int tcp_cancel ARGS(( int fd, int which_operation ));
void tcp_close ARGS(( int fd));

#endif /* TCP_H */

/*
 * $PchId: tcp.h,v 1.17 2005/06/28 14:20:54 philip Exp $
 */
