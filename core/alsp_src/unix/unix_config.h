#include "dfltsys.h"

#define OSStr "unix"

#define DIRENT	2
#define HAVE_FCNTL_H	2
#define HAVE_GETCWD	2
#define HAVE_GETHOSTNAME	2
#define HAVE_LGAMMA	2
#define HAVE_LIBM	2
#define HAVE_LONG_FILE_NAMES	2
#define HAVE_MKDIR	2
#define HAVE_MMAP	2
#define HAVE_MPROTECT	2
#define HAVE_NETDB_H	2
#define HAVE_REXEC	2
#define HAVE_SELECT	2
#define HAVE_SETITIMER	2
#define HAVE_SETSID	2
#define HAVE_SOCKET	2
#define HAVE_SRAND	2
#define HAVE_SRAND48	2
#define HAVE_STRCSPN	2
#define HAVE_STRDUP	2
#define HAVE_STRSPN	2
#define HAVE_STRTOK	2
#define HAVE_ST_BLKSIZE	2
#define HAVE_ST_BLOCKS	2
#define HAVE_ST_RDEV	2
#define HAVE_SYMLINK	2
#define HAVE_SYSCONF	2
#define HAVE_SYS_MMAN_H	2
#define HAVE_SYS_PARAM_H	2
#define HAVE_SYS_SOCKET_H	2
#define HAVE_SYS_STAT_H	2
#define HAVE_SYS_TIMES_H	2
#define HAVE_SYS_TIME_H	2
#define HAVE_SYS_WAIT_H	2
#define HAVE_TEMPNAM	2
#define HAVE_TERMIOS_H	2
#define HAVE_TERMIO_H	2
#define HAVE_TIME	2
#define HAVE_TIMES	2
#define HAVE_UNISTD_H	2
#define HAVE_VFPRINTF	2
#define HAVE_WAITPID	2
#define RETSIGTYPE	void
#define SIGRET	/*do nothing*/
#define SIGT	RETSIGTYPE
#define SIMPLE_MICS 2
#define TIME_WITH_SYS_TIME	2

#if   defined(UNIX_AIX)
#include "aix_config.h"
#elif defined(UNIX_SUNOS)
#include "sunos_config.h"
#elif defined(UNIX_SOLARIS)
#include "solaris_config.h"
#elif defined(UNIX_HPUX)
#include "hpux_config.h"
#elif defined(UNIX_LINUX)
#include "linux_config.h"
#elif defined(UNIX_IRIX)
#include "irix_config.h"
#elif defined(UNIX_CYGWIN32)
#include "cygwin32_config.h"
#else
#error
#endif
