
/* 
 * config.d.in is a template file used by configure to produce config.d.
 * config.d is then transformed (by configure) into the header file config.h.
 */

#define HP_AOUT_800 1
#define HAVE_MMAP_ZERO 1

#define MISSING_EXTERN_REALPATH
#define MISSING_EXTERN_H_ERRNO

/* wam.c won't compile with threading when using -fpic. Docs suggest
  that GCC 2.7 fixes this problem. */
#define Bytecode 1

#define MinorOSStr	"hpux9.05"
#define HAVE_MACHINE_PARAM_H	1
#define HAVE_MEMORY_H	1
#define HAVE_REGEX_H	1
#define HAVE_SGTTY_H	1
#define HAVE_DL_H	1
#define HAVE_LIBLD	1
#define HAVE_LIBPW	1
#define HAVE_LIBDLD	1
#define HAVE_BCOPY	1
#define HAVE_GAMMA	1
#define HAVE_MEMMOVE	1
#define HAVE_POLL	1
#define HAVE_SIGACTION	1
#define HAVE_SIGSTACK	1
#define HAVE_SIGVECTOR	1
#define HAVE_WAIT3	1
#define MISSING_EXTERN__FILBUF	1
#define MISSING_EXTERN__FLSBUF	1
#define MISSING_EXTERN_BCOPY	1
#define MISSING_EXTERN_BZERO	1
#define MISSING_EXTERN_GETPAGESIZE	1
#define MISSING_EXTERN_GETWD	1
#define MISSING_EXTERN_LDOPEN	1
#define MISSING_EXTERN_SIGVEC	1
