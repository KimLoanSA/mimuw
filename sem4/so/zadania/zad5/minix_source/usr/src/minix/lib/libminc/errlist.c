/* Automatically generated file; do not edit */
#include <sys/cdefs.h>
__RCSID("$NetBSD: errlist.awk,v 1.4 2010/12/16 22:52:32 joerg Exp $");
#include <errno.h>
static const char *const errlist[] = {
	"Undefined error: 0",			/* 0 - ENOERROR */
	"Operation not permitted",		/* 1 - EPERM */
	"No such file or directory",		/* 2 - ENOENT */
	"No such process",			/* 3 - ESRCH */
	"Interrupted system call",		/* 4 - EINTR */
	"Input/output error",			/* 5 - EIO */
	"Device not configured",		/* 6 - ENXIO */
	"Argument list too long",		/* 7 - E2BIG */
	"Exec format error",			/* 8 - ENOEXEC */
	"Bad file descriptor",			/* 9 - EBADF */
	"No child processes",			/* 10 - ECHILD */
	"Resource deadlock avoided",		/* 11 - EDEADLK */
	"Cannot allocate memory",		/* 12 - ENOMEM */
	"Permission denied",			/* 13 - EACCES */
	"Bad address",				/* 14 - EFAULT */
	"Block device required",		/* 15 - ENOTBLK */
	"Device busy",				/* 16 - EBUSY */
	"File exists",				/* 17 - EEXIST */
	"Cross-device link",			/* 18 - EXDEV */
	"Operation not supported by device",	/* 19 - ENODEV */
	"Not a directory",			/* 20 - ENOTDIR */
	"Is a directory",			/* 21 - EISDIR */
	"Invalid argument",			/* 22 - EINVAL */
	"Too many open files in system",	/* 23 - ENFILE */
	"Too many open files",			/* 24 - EMFILE */
	"Inappropriate ioctl for device",	/* 25 - ENOTTY */
	"Text file busy",			/* 26 - ETXTBSY */
	"File too large",			/* 27 - EFBIG */
	"No space left on device",		/* 28 - ENOSPC */
	"Illegal seek",				/* 29 - ESPIPE */
	"Read-only file system",		/* 30 - EROFS */
	"Too many links",			/* 31 - EMLINK */
	"Broken pipe",				/* 32 - EPIPE */
	"Numerical argument out of domain",	/* 33 - EDOM */
	"Result too large or too small",	/* 34 - ERANGE */
	"Resource temporarily unavailable",	/* 35 - EAGAIN */
	"Operation now in progress",		/* 36 - EINPROGRESS */
	"Operation already in progress",	/* 37 - EALREADY */
	"Socket operation on non-socket",	/* 38 - ENOTSOCK */
	"Destination address required",		/* 39 - EDESTADDRREQ */
	"Message too long",			/* 40 - EMSGSIZE */
	"Protocol wrong type for socket",	/* 41 - EPROTOTYPE */
	"Protocol option not available",	/* 42 - ENOPROTOOPT */
	"Protocol not supported",		/* 43 - EPROTONOSUPPORT */
	"Socket type not supported",		/* 44 - ESOCKTNOSUPPORT */
	"Operation not supported",		/* 45 - EOPNOTSUPP */
	"Protocol family not supported",	/* 46 - EPFNOSUPPORT */
	"Address family not supported by protocol family",/* 47 - EAFNOSUPPORT */
	"Address already in use",		/* 48 - EADDRINUSE */
	"Can't assign requested address",	/* 49 - EADDRNOTAVAIL */
	"Network is down",			/* 50 - ENETDOWN */
	"Network is unreachable",		/* 51 - ENETUNREACH */
	"Network dropped connection on reset",	/* 52 - ENETRESET */
	"Software caused connection abort",	/* 53 - ECONNABORTED */
	"Connection reset by peer",		/* 54 - ECONNRESET */
	"No buffer space available",		/* 55 - ENOBUFS */
	"Socket is already connected",		/* 56 - EISCONN */
	"Socket is not connected",		/* 57 - ENOTCONN */
	"Can't send after socket shutdown",	/* 58 - ESHUTDOWN */
	"Too many references: can't splice",	/* 59 - ETOOMANYREFS */
	"Operation timed out",			/* 60 - ETIMEDOUT */
	"Connection refused",			/* 61 - ECONNREFUSED */
	"Too many levels of symbolic links",	/* 62 - ELOOP */
	"File name too long",			/* 63 - ENAMETOOLONG */
	"Host is down",				/* 64 - EHOSTDOWN */
	"No route to host",			/* 65 - EHOSTUNREACH */
	"Directory not empty",			/* 66 - ENOTEMPTY */
	"Too many processes",			/* 67 - EPROCLIM */
	"Too many users",			/* 68 - EUSERS */
	"Disc quota exceeded",			/* 69 - EDQUOT */
	"Stale NFS file handle",		/* 70 - ESTALE */
	"Too many levels of remote in path",	/* 71 - EREMOTE */
	"RPC struct is bad",			/* 72 - EBADRPC */
	"RPC version wrong",			/* 73 - ERPCMISMATCH */
	"RPC prog. not avail",			/* 74 - EPROGUNAVAIL */
	"Program version wrong",		/* 75 - EPROGMISMATCH */
	"Bad procedure for program",		/* 76 - EPROCUNAVAIL */
	"No locks available",			/* 77 - ENOLCK */
	"Function not implemented",		/* 78 - ENOSYS */
	"Inappropriate file type or format",	/* 79 - EFTYPE */
	"Authentication error",			/* 80 - EAUTH */
	"Need authenticator",			/* 81 - ENEEDAUTH */
	"Identifier removed",			/* 82 - EIDRM */
	"No message of desired type",		/* 83 - ENOMSG */
	"Value too large to be stored in data type",/* 84 - EOVERFLOW */
	"Illegal byte sequence",		/* 85 - EILSEQ */
	"Not supported",			/* 86 - ENOTSUP */
	"Operation canceled",			/* 87 - ECANCELED */
	"Bad or Corrupt message",		/* 88 - EBADMSG */
	"No message available",			/* 89 - ENODATA */
	"No STREAM resources",			/* 90 - ENOSR */
	"Not a STREAM",				/* 91 - ENOSTR */
	"STREAM ioctl timeout",			/* 92 - ETIME */
	"Attribute not found",			/* 93 - ENOATTR */
	"Multihop attempted ",			/* 94 - EMULTIHOP */
	"Link has been severed",		/* 95 - ENOLINK */
	"Protocol error",			/* 96 - EPROTO */
	"Undefined error: 97",			/* 97 - UNDEFINED */
	"Undefined error: 98",			/* 98 - UNDEFINED */
	"Undefined error: 99",			/* 99 - UNDEFINED */
	"Undefined error: 100",			/* 100 - UNDEFINED */
	"Undefined error: 101",			/* 101 - UNDEFINED */
	"Undefined error: 102",			/* 102 - UNDEFINED */
	"Undefined error: 103",			/* 103 - UNDEFINED */
	"Undefined error: 104",			/* 104 - UNDEFINED */
	"Undefined error: 105",			/* 105 - UNDEFINED */
	"Undefined error: 106",			/* 106 - UNDEFINED */
	"Undefined error: 107",			/* 107 - UNDEFINED */
	"Undefined error: 108",			/* 108 - UNDEFINED */
	"Undefined error: 109",			/* 109 - UNDEFINED */
	"Undefined error: 110",			/* 110 - UNDEFINED */
	"Undefined error: 111",			/* 111 - UNDEFINED */
	"Undefined error: 112",			/* 112 - UNDEFINED */
	"Undefined error: 113",			/* 113 - UNDEFINED */
	"Undefined error: 114",			/* 114 - UNDEFINED */
	"Undefined error: 115",			/* 115 - UNDEFINED */
	"Undefined error: 116",			/* 116 - UNDEFINED */
	"Undefined error: 117",			/* 117 - UNDEFINED */
	"Undefined error: 118",			/* 118 - UNDEFINED */
	"Undefined error: 119",			/* 119 - UNDEFINED */
	"Undefined error: 120",			/* 120 - UNDEFINED */
	"Undefined error: 121",			/* 121 - UNDEFINED */
	"Undefined error: 122",			/* 122 - UNDEFINED */
	"Undefined error: 123",			/* 123 - UNDEFINED */
	"Undefined error: 124",			/* 124 - UNDEFINED */
	"Undefined error: 125",			/* 125 - UNDEFINED */
	"Undefined error: 126",			/* 126 - UNDEFINED */
	"Undefined error: 127",			/* 127 - UNDEFINED */
	"Undefined error: 128",			/* 128 - UNDEFINED */
	"Undefined error: 129",			/* 129 - UNDEFINED */
	"Undefined error: 130",			/* 130 - UNDEFINED */
	"Undefined error: 131",			/* 131 - UNDEFINED */
	"Undefined error: 132",			/* 132 - UNDEFINED */
	"Undefined error: 133",			/* 133 - UNDEFINED */
	"Undefined error: 134",			/* 134 - UNDEFINED */
	"Undefined error: 135",			/* 135 - UNDEFINED */
	"Undefined error: 136",			/* 136 - UNDEFINED */
	"Undefined error: 137",			/* 137 - UNDEFINED */
	"Undefined error: 138",			/* 138 - UNDEFINED */
	"Undefined error: 139",			/* 139 - UNDEFINED */
	"Undefined error: 140",			/* 140 - UNDEFINED */
	"Undefined error: 141",			/* 141 - UNDEFINED */
	"Undefined error: 142",			/* 142 - UNDEFINED */
	"Undefined error: 143",			/* 143 - UNDEFINED */
	"Undefined error: 144",			/* 144 - UNDEFINED */
	"Undefined error: 145",			/* 145 - UNDEFINED */
	"Undefined error: 146",			/* 146 - UNDEFINED */
	"Undefined error: 147",			/* 147 - UNDEFINED */
	"Undefined error: 148",			/* 148 - UNDEFINED */
	"Undefined error: 149",			/* 149 - UNDEFINED */
	"Undefined error: 150",			/* 150 - UNDEFINED */
	"Undefined error: 151",			/* 151 - UNDEFINED */
	"Undefined error: 152",			/* 152 - UNDEFINED */
	"Undefined error: 153",			/* 153 - UNDEFINED */
	"Undefined error: 154",			/* 154 - UNDEFINED */
	"Undefined error: 155",			/* 155 - UNDEFINED */
	"Undefined error: 156",			/* 156 - UNDEFINED */
	"Undefined error: 157",			/* 157 - UNDEFINED */
	"Undefined error: 158",			/* 158 - UNDEFINED */
	"Undefined error: 159",			/* 159 - UNDEFINED */
	"Undefined error: 160",			/* 160 - UNDEFINED */
	"Undefined error: 161",			/* 161 - UNDEFINED */
	"Undefined error: 162",			/* 162 - UNDEFINED */
	"Undefined error: 163",			/* 163 - UNDEFINED */
	"Undefined error: 164",			/* 164 - UNDEFINED */
	"Undefined error: 165",			/* 165 - UNDEFINED */
	"Undefined error: 166",			/* 166 - UNDEFINED */
	"Undefined error: 167",			/* 167 - UNDEFINED */
	"Undefined error: 168",			/* 168 - UNDEFINED */
	"Undefined error: 169",			/* 169 - UNDEFINED */
	"Undefined error: 170",			/* 170 - UNDEFINED */
	"Undefined error: 171",			/* 171 - UNDEFINED */
	"Undefined error: 172",			/* 172 - UNDEFINED */
	"Undefined error: 173",			/* 173 - UNDEFINED */
	"Undefined error: 174",			/* 174 - UNDEFINED */
	"Undefined error: 175",			/* 175 - UNDEFINED */
	"Undefined error: 176",			/* 176 - UNDEFINED */
	"Undefined error: 177",			/* 177 - UNDEFINED */
	"Undefined error: 178",			/* 178 - UNDEFINED */
	"Undefined error: 179",			/* 179 - UNDEFINED */
	"Undefined error: 180",			/* 180 - UNDEFINED */
	"Undefined error: 181",			/* 181 - UNDEFINED */
	"Undefined error: 182",			/* 182 - UNDEFINED */
	"Undefined error: 183",			/* 183 - UNDEFINED */
	"Undefined error: 184",			/* 184 - UNDEFINED */
	"Undefined error: 185",			/* 185 - UNDEFINED */
	"Undefined error: 186",			/* 186 - UNDEFINED */
	"Undefined error: 187",			/* 187 - UNDEFINED */
	"Undefined error: 188",			/* 188 - UNDEFINED */
	"Undefined error: 189",			/* 189 - UNDEFINED */
	"Undefined error: 190",			/* 190 - UNDEFINED */
	"Undefined error: 191",			/* 191 - UNDEFINED */
	"Undefined error: 192",			/* 192 - UNDEFINED */
	"Undefined error: 193",			/* 193 - UNDEFINED */
	"Undefined error: 194",			/* 194 - UNDEFINED */
	"Undefined error: 195",			/* 195 - UNDEFINED */
	"Undefined error: 196",			/* 196 - UNDEFINED */
	"Undefined error: 197",			/* 197 - UNDEFINED */
	"Undefined error: 198",			/* 198 - UNDEFINED */
	"Undefined error: 199",			/* 199 - UNDEFINED */
	"service restarted",			/* 200 - ERESTART */
	"source or destination is not ready",	/* 201 - ENOTREADY */
	"source or destination is not alive",	/* 202 - EDEADSRCDST */
	"pseudo-code: don't send a reply",	/* 203 - EDONTREPLY */
	"generic error",			/* 204 - EGENERIC */
	"invalid packet size for some protocol",/* 205 - EPACKSIZE */
	"urgent data present",			/* 206 - EURG */
	"no urgent data present",		/* 207 - ENOURG */
	"can't send message due to deadlock",	/* 208 - ELOCKED */
	"illegal system call number",		/* 209 - EBADCALL */
	"no permission for system call",	/* 210 - ECALLDENIED */
	"IPC trap not allowed",			/* 211 - ETRAPDENIED */
	"destination cannot handle request",	/* 212 - EBADREQUEST */
	"badmode in ioctl",			/* 213 - EBADMODE */
	"no such connection",			/* 214 - ENOCONN */
	"specified endpoint is not alive",	/* 215 - EDEADEPT */
	"specified endpoint is bad",		/* 216 - EBADEPT */
	"requested CPU does not work",		/* 217 - EBADCPU */
};

const int sys_nerr = sizeof(errlist) / sizeof(errlist[0]);
const char * const *sys_errlist = errlist;
