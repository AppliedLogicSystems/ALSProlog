/*=======================================================================*
 |		wntbl.h	
 |	Copyright (c) 1989-1995 Applied Logic Systems, Inc.
 |
 |		-- machine specific name table entry sizes for wintcode.h
 |
 | Creation: 5/5/89
 | Author: Kevin Buettner and Keith Hughes
 | 
 | NTBL_HEADERSIZE gives the number of words occupied by the structure 
 | prior to the overflow entry code.
 |
 | NTBL_OVERFLOWSIZE gives the number of words needed for a jsr to the
 | overflow code.
 |
 | NTBL_CALLENTRYSIZE gives the number of words needed for the link 
 | instruction at the call entry point.
 |
 | NTBL_EXECENTRYSIZE gives the number of words needed for the overflow 
 | checking code.  This, coincidentally, is also the number of words 
 | needed for a jsr to the spy handler.
 |
 | NTBL_CODESIZE is the total number of words for the rest of the entry 
 | point which will include the conditional branch to the overflow code 
 | and any other stuff which needs to happen afterwards.
 |
 | The entry point is arranged (roughly) as follows (68020):
 |
 |      overflow_entry:
 |              jsr     overflow
 |              bra.s   code
 |      call_entry:
 |              link    E, #0
 |      exec_entry:
 |              move.l  TR, D0
 |              sub.l   H, D0
 |              cmp.l   SAF, D0
 |      code:
 |              blo.s   overflow_entry
 |              .
 |              .
 |              .
 *=======================================================================*/

#ifdef Portable
#ifdef __LP64__
#define NTBL_HEADERSIZE         ((7*sizeof(Code)+4*sizeof(short))/sizeof(Code))
#else
#define NTBL_HEADERSIZE         (36/sizeof(Code))
#endif
#define NTBL_OVERFLOWSIZE       1
#define NTBL_CALLENTRYSIZE      1
#define NTBL_EXECENTRYSIZE      1
#define NTBL_CODESIZE           (1+4*sizeof(long)/sizeof(Code))
#endif /* Portable */

#ifdef arch_i386
	/* If want inline choice points,
#define INLINECHOICE
	*/

/* If these change, wntbl.m4 must be updated */

/*
Size in Code ops of the stuff that comes before the code in the
ntbl_entry struct above. Code ops on 386 are bytes.
*/

#define NTBL_HEADERSIZE		36
#define NTBL_OVERFLOWSIZE	6
#define NTBL_CALLENTRYSIZE	1
#define NTBL_EXECENTRYSIZE	14
#define NTBL_CODESIZE		84

#endif /* arch_i386 */

#ifdef arch_m68k

#define NTBL_HEADERSIZE		18
#define NTBL_OVERFLOWSIZE	4
#define NTBL_CALLENTRYSIZE	2
#define NTBL_EXECENTRYSIZE	3

#ifdef MacOS
#define NTBL_CODESIZE       35
#else
#define NTBL_CODESIZE		29
#endif /* MacOS */

#endif /* arch_m68k */


#ifdef arch_m88k

#define NTBL_HEADERSIZE		9
#define NTBL_OVERFLOWSIZE	2
#define NTBL_CALLENTRYSIZE	2
#ifndef HAVE_MMAP
#define NTBL_EXECENTRYSIZE	4
#else /* HAVE_MMAP */
#define NTBL_EXECENTRYSIZE	3
#endif /* HAVE_MMAP */
#define NTBL_CODESIZE		19

#endif /* arch_m88k */

#ifdef arch_vax

#define NTBL_HEADERSIZE		36
#define NTBL_OVERFLOWSIZE	8
#define NTBL_CALLENTRYSIZE	5
#define NTBL_EXECENTRYSIZE	7
#define NTBL_CODESIZE		52

#endif /* arch_vax */

#ifdef arch_sparc

#define NTBL_HEADERSIZE		9
#define NTBL_OVERFLOWSIZE	2
#define NTBL_CALLENTRYSIZE	1
#define NTBL_EXECENTRYSIZE	4		/* was 6 (12-21-92) */
#define NTBL_CODESIZE		28		/* bigger than necessary */

#endif /* arch_sparc */
