
/* 
 * config.d.in is a template file used by configure to produce config.d.
 * config.d is then transformed (by configure) into the header file config.h.
 */

#define SIMPLE_MICS 1
#define HAVE_LIBELF	1
#define USE_ELF_SECTION_FOR_IMAGE 1
#define MISSING_EXTERN_GETHOSTNAME
#define MISSING_EXTERN_GETIMEOFDAY

/* define __EXTENSIONS__ to get crypt() prototype */
#define __EXTENSIONS__

/* Solaris does not have the standard regcomp() library, so use
   regcmp().
*/
#define USE_REGCMP 1

#define MinorOSStr	"solaris2.4"
#define HAVE_IEEE_FP 1

#define DIRENT	1
#define HAVE_FCNTL_H	1
#define HAVE_NETDB_H	1
#define HAVE_SGTTY_H	1
#define HAVE_SYS_MMAN_H	1
#define HAVE_SYS_PARAM_H	1
#define HAVE_SYS_SELECT_H	1
#define HAVE_SYS_SOCKET_H	1
#define HAVE_SYS_STAT_H	1
#define HAVE_SYS_TIME_H	1
#define HAVE_SYS_TIMES_H	1
#define HAVE_SYS_WAIT_H	1
#define HAVE_TERMIOS_H	1
#define HAVE_TERMIO_H	1
#define HAVE_UCONTEXT_H	1
#define HAVE_UNISTD_H	1
#define HAVE_DLFCN_H	1
#define RETSIGTYPE	void
#define SIGT	RETSIGTYPE
#define SIGRET	/*do nothing*/
#define HAVE_ST_BLKSIZE	1
#define HAVE_ST_BLOCKS	1
#define HAVE_ST_RDEV	1
#define TIME_WITH_SYS_TIME	1
#define HAVE_LONG_FILE_NAMES	1
#define HAVE_LIBDL	1
#define HAVE_LIBM	1
#define HAVE_LIBGEN	1
#define HAVE_LIBNSL	1
#define HAVE_LIBSOCKET	1
#define HAVE_DEV_ZERO	1
#define HAVE_DLOPEN	1
#define HAVE_GAMMA	1
#define HAVE_GETCWD	1
#define HAVE_GETHOSTNAME	1
#define HAVE_LGAMMA	1
#define HAVE_MEMMOVE	1
#define HAVE_MKDIR	1
#define HAVE_MMAP	1
#define HAVE_MPROTECT	1
#define HAVE_POLL	1
#define HAVE_REGCMP	1
#define HAVE_REXEC	1
#define HAVE_RINT	1
#define HAVE_SELECT	1
#define HAVE_SETITIMER	1
#define HAVE_SETSID	1
#define HAVE_SIGACTION	1
#define HAVE_SIGALTSTACK	1
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
#define HAVE_WAITPID	1

