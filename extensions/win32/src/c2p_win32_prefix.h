
/* Preprocessor symbols necessary for ANSI headers. */
#define __MWERKS__ 1
#define __INTEL__ 1

#include <x86_prefix.h>
/* Preprcessor symbols from x86_prefix.h - all specifiers produce nothing. */

/* dummy specifiers (should be incorportated in c2pro. */
#define __declspec(X)
#define _huge
#define __cdecl
#define __stdcall

/* Some defined and includes needed my all windows headers. */
#ifndef WINVER
#define WINVER 0x0400
#endif

#define _X86_

#exclude <excpt.h>
#include <excpt.h>
#exclude <stdarg.h>
#include <stdarg.h>


