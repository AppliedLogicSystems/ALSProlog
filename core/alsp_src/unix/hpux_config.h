
/* 
 * config.d.in is a template file used by configure to produce config.d.
 * config.d is then transformed (by configure) into the header file config.h.
 */

#define SIMPLE_MICS 1
#define HP_AOUT_800 1
#define HAVE_MMAP_ZERO 1


/* wam.c won't compile with threading when using -fpic. Docs suggest
  that GCC 2.7 fixes this problem. */
#define Bytecode 1

#define MinorOSStr	"hpux9.05"
#define DIRENT	1
#define HAVE_FCNTL_H	1
#define HAVE_MACHINE_PARAM_H	1
#define HAVE_MEMORY_H	1
#define HAVE_NETDB_H	1
#define HAVE_REGEX_H	1
#define HAVE_SGTTY_H	1
#define HAVE_SYS_MMAN_H	1
#define HAVE_SYS_PARAM_H	1
#define HAVE_SYS_SOCKET_H	1
#define HAVE_SYS_STAT_H	1
#define HAVE_SYS_TIME_H	1
#define HAVE_SYS_TIMES_H	1
#define HAVE_SYS_WAIT_H	1
#define HAVE_TERMIOS_H	1
#define HAVE_TERMIO_H	1
#define HAVE_UNISTD_H	1
#define HAVE_DL_H	1
#define RETSIGTYPE	void
#define SIGT	RETSIGTYPE
#define SIGRET	/*do nothing*/
#define HAVE_ST_BLKSIZE	1
#define HAVE_ST_BLOCKS	1
#define HAVE_ST_RDEV	1
#define TIME_WITH_SYS_TIME	1
#define HAVE_LONG_FILE_NAMES	1
#define HAVE_LIBLD	1
#define HAVE_LIBPW	1
#define HAVE_LIBM	1
#define HAVE_LIBDLD	1
#define HAVE_BCOPY	1
#define HAVE_GAMMA	1
#define HAVE_GETCWD	1
#define HAVE_GETHOSTNAME	1
#define HAVE_LGAMMA	1
#define HAVE_MEMMOVE	1
#define HAVE_MKDIR	1
#define HAVE_MMAP	1
#define HAVE_MPROTECT	1
#define HAVE_POLL	1
#define HAVE_REXEC	1
#define HAVE_SELECT	1
#define HAVE_SETITIMER	1
#define HAVE_SETSID	1
#define HAVE_SIGACTION	1
#define HAVE_SIGSTACK	1
#define HAVE_SIGVECTOR	1
#define HAVE_SOCKET	1
#define HAVE_SRAND	1
#define HAVE_SRAND48	1
#define HAVE_STRCSPN	1
#define HAVE_STRDUP	1
#define HAVE_STRSPN	1
#define HAVE_STRTOK	1
#define HAVE_SYMLINK	1
#define HAVE_SYSCONF	1
#define HAVE_TEMPNAM	1
#define HAVE_TIME	1
#define HAVE_TIMES	1
#define HAVE_VFPRINTF	1
#define HAVE_WAIT3	1
#define HAVE_WAITPID	1
#define MISSING_EXTERN__FILBUF	1
#define MISSING_EXTERN__FLSBUF	1
#define MISSING_EXTERN_BCOPY	1
#define MISSING_EXTERN_BZERO	1
#define MISSING_EXTERN_GETPAGESIZE	1
#define MISSING_EXTERN_GETWD	1
#define MISSING_EXTERN_LDOPEN	1
#define MISSING_EXTERN_SIGVEC	1
