#define MinorOSStr	"darwin"

// ASLR/PIE can't be disabled on Apple Silicon M1, so use Bytecode
#ifdef __arm64__
#define Bytecode 1
#endif

#undef HAVE_REXEC
#define EXTERNAL_STATE 1
#define MAP_ANONYMOUS MAP_ANON
#define HAVE_DLFCN_H	1
#if 0

#define HAVE_MEMORY_H	1
#define HAVE_LIBDL	1
/*#define HAVE_LIBELF	1*/
#define HAVE_DEV_ZERO	1
#define HAVE_BCOPY	1
#define HAVE_DLOPEN	1
#define HAVE_GETPAGESIZE	1
#define HAVE_MEMMOVE	1
#define HAVE_RE_COMP	1
#define HAVE_RINT	1
#define HAVE_SIGACTION	1
#define HAVE_SRANDOM	1
#define HAVE_WAIT3	1
#endif
