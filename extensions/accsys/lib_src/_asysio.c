/********************************
*          _asysio.c		*
*********************************/

/************************************************************
*                                                           *
* Copyright 1990, 1991, Billy Shakespeare & Company, Inc.   *
* All rights reserved.                                      *
*                                                           *
*************************************************************
*                                                           *
* Published by                                              *
*        Copia International, Inc.                          *
*        Wheaton, Illinois                                  *
*        U. S. A.                                           *
*                                                           *
*************************************************************/

#include <errno.h>
#include <stdlib.h>

#ifdef TURBOC
#include <ctype.h>
#include <dos.h>
#endif

#ifdef LATTICE
#include <dos.h>
#endif

#include "db4.h"
#include "d4def.h"

#ifdef STANDARD
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#endif

#ifdef ZORTECH
#define USEINTREG
#endif

#include <fcntl.h>


#if defined(OS2)

#define INCL_DOS     /* Microsoft C only */
#include <os2def.h>  /* Microsoft C only */
#include <bsedos.h>  /* Microsoft C only */

	/* If the following symbols are not defined, define them.
	 *		(for MS-C 5.1)
	 */
#ifndef FILE_NORMAL
#define FILE_NORMAL 0x0000
#endif

#ifndef FILE_CREATE
#define FILE_CREATE 0x0010
#endif

#ifndef FILE_TRUNCATE
#define FILE_TRUNCATE 0x0002
#endif

#ifndef OPEN_ACCESS_READWRITE
#define OPEN_ACCESS_READWRITE 0x0002
#endif

#ifndef OPEN_SHARE_DENYREADWRITE
#define OPEN_SHARE_DENYREADWRITE 0x0010
#endif



#elif defined(unix)
#include <sys/types.h>
#include <sys/stat.h>

#elif defined(MacOS)
/* Undefine OPEN because it conflicts with a struct definition in AppleTalk.h */
#undef OPEN
#include <GUSI.h>

#else
	/* PC-DOS or MS-DOS */
#include <dos.h>
#ifndef _A_NORMAL
#define _A_NORMAL 0
	/* Some compiler package (e.g., Turbo C 2.0)
	   does not have this definition. */
#endif
#endif	/* OS2, unix, MacOS */

#include "db4.h"
#include "d4def.h"

int _DECLARE _dcre8new(filename,force)
		/* input */
	CHAR_PTR filename; 	/* full filename */
	int  force;  		/*  FORCE:  create even if the file already exists
		        			~FORCE: fail if the file exists */
		/* output: none */
		/* returns: file handle, or -1 on error */
{
	int handle;
#ifdef _WINDOWS
	OFSTRUCT wrc;
#endif
#ifdef OS2
	USHORT usAction;
#endif
#ifdef USEINTREG
	union REGS inregs, outregs;
	struct SREGS segregs;
#endif

if (force != FORCE)
{
#if defined(TURBOC)
  	_fmode = O_BINARY;
	handle = creatnew(filename, _A_NORMAL);

#elif defined(MSC)
#if defined(OS2)
    if (DosOpen((PSZ) filename, (PHFILE) &handle, &usAction,
    			0L, FILE_NORMAL, FILE_CREATE,
			OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYREADWRITE,
			0L)
       ) handle = -1;
#elif defined(_WINDOWS)
	handle = OpenFile((LPSTR) filename, (LPOFSTRUCT) &wrc,
		OF_EXIST | OF_SHARE_EXCLUSIVE);
	if (handle) goto done;
	handle = OpenFile((LPSTR) filename,  (LPOFSTRUCT) &wrc,
			OF_CREATE | OF_READWRITE | OF_SHARE_EXCLUSIVE);
#else
	/* PC-DOS || MS-DOS */
	if (_dos_creatnew(filename, _A_NORMAL, &handle)) handle = -1;
#endif /* OS2, _WINDOWS */

#elif defined(USEINTREG)
	inregs.h.ah = 0x5b; /* create new file */
	inregs.x.cx = 0;    /* CX.HIGH = 0   CX.LOW = attribute */
	inregs.x.dx = FP_OFF((char far *) filename);
	segregs.ds  = FP_SEG((char far *) filename);

	handle = intdosx(&inregs, &outregs, &segregs);
	if (!(outregs.x.flags & 0x01)) /* check carry */
		return(handle);

	/* get extended error code */
	inregs.h.ah = 0x59; /* get extended error */
	inregs.x.bx = 0;    /* CX.HIGH = 0   CX.LOW = 0 */

	(void) intdos(&inregs, &outregs);
	errno = _doserrno;
	handle = -1;

#elif defined(LATTICE)
	handle = dcreatx(filename, _A_NORMAL);

#elif defined(__HIGHC__)
	if (_dos_creatnew(filename, _A_NORMAL, &handle)) handle = -1;

#elif defined(unix)
	handle = open(filename, O_RDWR | O_CREAT | O_EXCL | O_TRUNC);

#elif defined(MacOS)
	handle = open(filename, O_RDWR | O_CREAT | O_EXCL | O_TRUNC | O_BINARY);
	fsetfileinfo(filename, '????', '????');
#else
#error
#endif 	/* TURBOC, MSC, USEINTREG, LATTICE, _HIGHC_, unix, MacOS */

done:
    return(handle);
}

/* force == FORCE */
#if defined(TURBOC)
    handle = _creat(filename, 0);

#elif defined(MSC)
#if defined(OS2)
	    if (DosOpen((PSZ) filename, (PHFILE) &handle, &usAction,
    			0L, FILE_NORMAL, FILE_TRUNCATE | FILE_CREATE,
			OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYREADWRITE,
			0L)
	       ) handle = -1;

#elif defined(_WINDOWS)
		handle = OpenFile((LPSTR) filename,  (LPOFSTRUCT) &wrc,
			OF_CREATE | OF_READWRITE | OF_SHARE_EXCLUSIVE);
#else
		/* PC-DOS || MS-DOS */
		if (_dos_creat(filename, _A_NORMAL, &handle)) handle = -1;
#endif

#elif defined(ZORTECH)
    handle = creat(filename, S_IREAD | S_IWRITE);

#elif defined(LATTICE)
    handle = creat(filename, S_IREAD | S_IWRITE | O_RAW);

#elif defined(__HIGHC__)
	if (_dos_creat(filename, _A_NORMAL, &handle)) handle = -1;

#elif defined(unix)
	handle = open(filename, O_RDWR | O_CREAT | O_TRUNC,
				  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);

#elif defined(MacOS)
	handle = open(filename, O_RDWR | O_CREAT | O_TRUNC | O_BINARY);
#else
#error
#endif	/* TURBOC, MSC, USEINTREG, LATTICE, _HIGHC_, unix, MacOS */

	return(handle);
} /* end of _dcre8new() */

