
/* 
 * config.d.in is a template file used by configure to produce config.d.
 * config.d is then transformed (by configure) into the header file config.h.
 */

/* For now, use external state files, because the addition of the threads
library to the link line for the Java interface produces freezes in the
libelf save-state code. */
#define EXTERNAL_STATE 1
/*#define HAVE_LIBELF	1*/
/*#define USE_ELF_SECTION_FOR_IMAGE 1*/
#define MISSING_EXTERN_GETHOSTNAME
#define MISSING_EXTERN_GETIMEOFDAY

#define MISSING_GLOB 1


/* define __EXTENSIONS__ to get crypt() prototype */
#define __EXTENSIONS__

/* Solaris does not have the standard regcomp() library, so use
   regcmp().
*/
#define USE_REGCMP 1

#define MinorOSStr	"solaris2.4"
#define HAVE_IEEE_FP 1

#define HAVE_SGTTY_H	1
#define HAVE_SYS_SELECT_H	1
#if 0
#define HAVE_UCONTEXT_H	1
#endif
#define HAVE_DLFCN_H	1
#define HAVE_LIBDL	1
#define HAVE_LIBGEN	1
#define HAVE_LIBNSL	1
#define HAVE_LIBSOCKET	1
#define HAVE_DEV_ZERO	1
#define HAVE_DLOPEN	1
#define HAVE_GAMMA	1
#define HAVE_MEMMOVE	1
#define HAVE_POLL	1
#define HAVE_REGCMP	1
#define HAVE_RINT	1
#if 0
#define HAVE_SIGACTION	1
#define HAVE_SIGALTSTACK	1
#endif

