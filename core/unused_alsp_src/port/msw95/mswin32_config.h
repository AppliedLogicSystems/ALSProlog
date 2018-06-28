/* 
 *  mswin32_config.h	Hand made MSWin32 configuration file.
 *	Copyright (c) 1996 Applied Logic Systems, Inc.
 *
 *  Author:	Chuck Houpt
 *  Creation:	1/30/96
 *  Revision History:
 *	Revised: mm/dd/yy	Who		-- Why and What
 */

#define HAVE_STDARG_H	1
#define HAVE_STDLIB_H	1
#define HAVE_FCNTL_H	1
#define HAVE_SRAND		1

#define HAVE_TIME		1

#define HAVE_SOCKET		1
#define BERKELEY_SOCKETS	1
#define HAVE_SELECT		1

#define PARAMREVBIT 1

#define APP_PRINTF_CALLBACK	1
//#undef FSACCESS

#define SIMPLE_MICS	1
#if 0

#define MISSING_EXTERN_ACCESS	1
#define MISSING_EXTERN_CHDIR	1
#define MISSING_EXTERN_GETCWD	1
#define MISSING_EXTERN_LSEEK	1
#define MISSING_EXTERN_OPEN		1
#define MISSING_EXTERN_READ		1
#define MISSING_EXTERN_WRITE	1
#define MISSING_EXTERN_CLOSE	1
#define MISSING_EXTERN_UNLINK	1

#define STDC_HEADERS	1


#define HAVE_STDDEF_H	1


#define HAVE_MEMMOVE	1


#define HAVE_VFPRINTF	1
#endif
