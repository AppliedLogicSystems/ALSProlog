/*=====================================================================*
 |
 |	ole32.h
 |	Copyright (c) 1994-96 Applied Logic Systems, Inc.
 |
 *=====================================================================*/

/* Stop Inline Assembly. */
#define NO_INLINE_FUNCTIONS 1

//#include <windef.h>
//#include <winbase.h>
//#include <wingdi.h>

//#include <ole2.h>

#include <windows.h>

/* PROCESSOR_ARCHITECTURE_UNKNOWN is defined as 0xFFFF;
   The semi-colon is probably just a typo, so redefine it here. */
 
#ifdef PROCESSOR_ARCHITECTURE_UNKNOWN
#undef PROCESSOR_ARCHITECTURE_UNKNOWN
#define PROCESSOR_ARCHITECTURE_UNKNOWN 0xFFFF
#endif
