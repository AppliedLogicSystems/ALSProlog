/*
 * wci.h	-- machine specific clause index information
 *	Copyright (c) 1989-1993 by Applied Logic Systems, Inc.
 *
 * Author: Keith Hughes and Kevin Buettner
 * Creation Date: 5/5/89
 *
 * Revision History:
 *   Revised:	mm/dd/yy	Who		What and Why
 *   Revised:	mm/dd/yy	Who		What and Why
 *
 *
 *
 * This file is included in wintcode.h.  It completes the clause header
 * index information.
 *
 * WCI_CHOICEENTRY is the index into the clause giving the entry point
 *	for the choice point instruction.  Often this is simply where
 *	the choice point code starts.
 *
 * WCI_CLAUSECODE is the index into the clause giving the actual start
 *	of the clause code.
 *
 */

#ifdef Portable

#define WCI_CHOICEENTRY         (WCI_CHOICECODE)
#define WCI_CLAUSECODE          (WCI_CHOICECODE+2)

#endif /* Portable */

#ifdef arch_i386

#define WCI_CHOICEENTRY		(WCI_CHOICECODE)
#define WCI_CLAUSECODE		(WCI_CHOICECODE+3)

#endif /* arch_i386 */


#ifdef arch_m68k

#define WCI_CHOICEENTRY		(WCI_CHOICECODE+1)
#ifdef MacOS
#define WCI_CLAUSECODE      (WCI_CHOICECODE+10)
#else
#define WCI_CLAUSECODE		(WCI_CHOICECODE+7)
#endif /* MacOS */

#endif /* arch_m68k */


#ifdef arch_m88k

#define WCI_CHOICEENTRY		(WCI_CHOICECODE)
#define WCI_CLAUSECODE		(WCI_CHOICECODE+4)

#endif /* arch_m88k */


#ifdef arch_vax

#define WCI_CHOICEENTRY		(WCI_CHOICECODE)
#define WCI_CLAUSECODE		(WCI_CHOICECODE+3)

#endif /* arch_vax */


#ifdef arch_sparc

#define WCI_CHOICEENTRY		(WCI_CHOICECODE)
#define WCI_CLAUSECODE		(WCI_CHOICECODE+4)

#endif	/* arch_sparc */
