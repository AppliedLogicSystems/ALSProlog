/*=====================================================================*
 |
 |	mswin32.spc
 |	Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *=====================================================================*/

//#exclude <stdarg.h>
//#exclude <ctype.h>
//#exclude <string.h>

/* Preprocessor symbols necessary for ANSI headers. */
#define __MWERKS__ 1
#define __INTEL__ 1

/* Preprcessor symbols from x86_prefix.h - all specifiers produce nothing. */
#define _X86_
#define _M_IX86 300
#define WIN32 1
#define _WIN32
#define _MSC_VER 900

#define _cdecl
#define __cdecl
#define _asm

#define _stdcall
#define __stdcall
#define _CRTAPI1

/* More dummy specifiers. */
#define __declspec(X)
#define _huge


#include "mswin32.h"

