/*=================================================================*
 |			alstypes.h	
 |		Copyright (c) 1988-1993 by Applied Logic Systems, Inc.
 |
 |			-- Define object types for the system
 |
 | Author: Keith M. Hughes
 | Creation Date: 12/88
 | Revision History:
 | 04/07/89 - K.Buettner -- 88k, SUN and 68020 additions
 | 04/25/89 - K.Buettner -- Merged in Keith's code.h file
 | 11/20/94 - C. Houpt	 -- Renamed alstype.h to avoid Mac system 
 |                          header conflict.
 *=================================================================*/
#ifndef ALS_TYPES_INCLUDED	/* ALS_ prepended to avoid confusion */

#define ALS_TYPES_INCLUDED

#ifndef PWordTypeDefined
typedef long PWord;
#define PWordTypeDefined 1
#endif

/*
 * Code typedef; 
 * This is the entity that a basic machine instruction or
 * opcode fits into.  The values for the machines under consideration are:
 *	88k	-- long (32 bits)
 *	68020	-- short (16 bits)
 *	80386	-- char (8 bits)
 *	vax	-- char (8 bits)
 *	sparc	-- long (32 bits)
 */

#ifdef Portable
#ifdef LongCode
typedef long Code;
#else
#ifdef ShortCode
typedef short Code;
#else
typedef char Code;
#endif
#endif
#endif /* Portable */

#ifdef arch_m88k
typedef long Code;
#endif
#ifdef arch_i386
typedef char Code;
#endif
#ifdef arch_m68k
typedef short Code;
#endif
#ifdef arch_vax
typedef unsigned char Code;
#endif
#ifdef arch_sparc
typedef long Code;
#endif


typedef long Offset;
typedef char SmallOffset;
typedef long BigOffset;

typedef Code *CodePtr;
#define CodeSize sizeof(Code)

#ifndef MSWin32
/* Also defined by Win32 headers. */
typedef unsigned char UCHAR;	/* /usr/include/sys/types.h defines uchar_t */
#endif

#endif /* ALS_TYPES_INCLUDED */
