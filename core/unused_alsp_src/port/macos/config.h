/* 
 *  macconfig.h	Hand made Macintosh configuration file.
 *	Copyright (c) 1995 Applied Logic Systems, Inc.
 *
 *  Author:	Chuck Houpt
 *  Creation:	1/5/95
 *  Revision History:
 *	Revised: mm/dd/yy	Who		-- Why and What
 */

#ifdef UNIX
#undef UNIX
#endif

#ifdef OSStr
#undef OSStr
#endif

#ifdef HAVE_BRK
#undef HAVE_BRK
#endif

#define OSStr "macos"
#define MinorOSStr	"macintosh"
#define MacOS		1

#define STDC_HEADERS	1

#ifdef THINK_C
#define HAVE_FCNTL_H	1
#endif
#ifdef applec
#define HAVE_FCNTL_H	1
#endif

#define HAVE_STDARG_H	1
#define HAVE_STDDEF_H	1
#define HAVE_STDLIB_H	1
#define HAVE_STRING_H	1

// GUSI
//#define pid_t	int

#define HAVE_MEMMOVE	1

#ifdef MPW_TOOL
#define HAVE_RINT	1
#endif

#define HAVE_SRAND	1

#define HAVE_STRCSPN	1
#define HAVE_STRSPN	1
#define HAVE_STRTOK	1

#define HAVE_TIME	1

#define HAVE_VFPRINTF	1

// defs for GUSI
#define HAVE_FCNTL_H	1
#define HAVE_SOCKET		1
#define HAVE_UNISTD_H	1
//#define HAVE_SYS_TYPES_H	1
//#define HAVE_SYS_STAT_H 1