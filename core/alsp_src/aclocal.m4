dnl
dnl Local definitions for autoconf.
dnl
dnl VC_HAVE_LIBRARY is a slightly modifid version of AC_HAVE_LIBRARY from 
dnl ac_general.m4.  This version does precisely what AC_HAVE_LIBRARY does
dnl but builds up the library list in reverse order so that checks for
dnl libraries which depend on other libraries won't fail for the wrong reason.
dnl
dnl Thus the strategy is to check for the more general libraries first and
dnl the libraries which depend on the more general libraries last.
dnl
define(VC_HAVE_LIBRARY, [dnl
changequote(/,/)dnl
define(/VC_LIB_NAME/, dnl
patsubst(patsubst($1, /lib\([^\.]*\)\.a/, /\1/), /-l/, //))dnl
changequote([,])dnl
ac_save_LIBS="${LIBS}"
LIBS="-l[]VC_LIB_NAME[] ${LIBS}"
ac_have_lib=""
AC_COMPILE_CHECK([-l[]VC_LIB_NAME[]], , [main();], [ac_have_lib="1"])dnl
LIBS="${ac_save_LIBS}"
ifelse($#, 1, [dnl
if test -n "${ac_have_lib}"; then
   AC_DEFINE([HAVE_LIB]translit(VC_LIB_NAME, [a-z], [A-Z]))
   LIBS="-l[]VC_LIB_NAME[] ${LIBS}"
fi
undefine(VC_LIB_NAME)dnl
], [dnl
if test -n "${ac_have_lib}"; then
   :; $2
else
   :; $3
fi
])])dnl
dnl
dnl----------------------------
define(VC_SETPGRP,
[AC_CHECKING([for BSD style setpgrp])
AC_TEST_PROGRAM([
main()
{
    if (setpgrp(1,1) == -1)
	exit(0);
    else
	exit(1);
}],AC_DEFINE(HAVE_BSD_SETPGRP))
])dnl
dnl
dnl----------------------------
define(VC_KILLPG,
[AC_REQUIRE([AC_RETSIGTYPE])
AC_REQUIRE([VC_SETPGRP])
AC_CHECKING([if killpg is needed])
AC_TEST_PROGRAM([
#include <sys/types.h>
#include <signal.h>
RETSIGTYPE
handler(s)
    int s;
{
    exit(0);
}

main()
{
#ifdef HAVE_BSD_SETPGRP
    (void) setpgrp(0,0);
#else
    (void) setpgrp();
#endif
    (void) signal(SIGINT, handler);
    (void) kill(-getpid(), SIGINT);
    exit(1);
}], ,AC_DEFINE(HAVE_KILLPG))
])dnl
dnl
dnl----------------------------
define(VC_MISSING_CHECK,
[AC_COMPILE_CHECK([missing "$1" extern],
[
#include <stdio.h>
#include <sys/types.h>
#include <setjmp.h>
#include <signal.h>
#include <errno.h>
#ifdef HAVE_TYPES_H
#include <types.h>
#endif
#ifdef HAVE_LIBC_H
#include <libc.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#ifdef HAVE_VARARGS_H
#include <varargs.h>
#endif
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#if STDC_HEADERS || HAVE_STRING_H
#include <string.h>
  /* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !STDC_HEADERS && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#if HAVE_STRINGS_H
#include <strings.h>
  /* memory.h and strings.h conflict on some systems */
#endif
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

/* unistd.h defines _POSIX_VERSION on POSIX.1 systems.  */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#else /* not (DIRENT or _POSIX_VERSION) */
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* not (DIRENT or _POSIX_VERSION) */

#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#ifdef HAVE_STAT_H
#include <stat.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif
#ifdef HAVE_SIGINFO_H
#include <siginfo.h>
#endif

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#else
#ifdef HAVE_REGEX_H
#include <regex.h>
#endif
#endif
#ifdef HAVE_MATH_H
#include <math.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif
#ifdef HAVE_SYS_MSG_H
#include <sys/msg.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef HAVE_SELECT_H
#include <select.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_LDFCN_H
#include <filehdr.h>
#include <ldfcn.h>
#endif
#undef $1
struct zowie { int a; double b; struct zowie *c; char d; };
extern struct zowie *$1();
],
[int i;],
[AC_DEFINE(MISSING_EXTERN_$2)],
[]
)]
)dnl
dnl
define(VC_MISSING_EXTERN,
[for ac_func in $1
do
changequote(,)dnl
ac_tr_func=`echo $ac_func | tr '[a-z]' '[A-Z]'`
changequote([,])dnl
VC_MISSING_CHECK(${ac_func}, ${ac_tr_func})dnl
done
])dnl
dnl
