/* 
 *  mswin32_config.h	Hand made MSWin32 configuration file.
 *	Copyright (c) 1996 Applied Logic Systems, Inc.
 *
 *  Author:	Chuck Houpt
 *  Creation:	1/30/96
 */

#include "dfltsys.h"

#define MSWin32 1
#define OSStr "mswin32"

#ifdef __GNUC__
#define EXTERNAL_STATE	1
#endif

/* Temp. disable threading until threading GUI stub is fixed */
#ifdef __GNUC__
#define Bytecode 1
#endif

#define HAVE_STDARG_H	1
#define HAVE_STDLIB_H	1
#define HAVE_FCNTL_H	1
#define HAVE_SRAND		1

#define HAVE_TIME		1

#define HAVE_SOCKET		1
#define BERKELEY_SOCKETS	1
#define HAVE_SELECT		1
#define MISSING_UNIX_DOMAIN_SOCKETS 1

#define APP_PRINTF_CALLBACK	1

#define REVERSE_ENDIAN 1

#include <winsock2.h>
#include <windows.h>
