/*====================================================================*
 *			chpt.h
 *	Copyright (c) 1987-1994, Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Created: 9/23/87 (out of a piece of gc.c)
 *
 * Note:  This file should probably be split up and put into the system
 * specific directories.
 *====================================================================*/

#ifndef _CHPT_H_INCLUDED_
#define _CHPT_H_INCLUDED_ 1

/*
 * Maximum size of an index block (long words)
 *
 * This is the value that the index code (index.c) will use to determine
 * whether to allocate space or forget about indexing.  We could probably
 * achieve the same effect by limiting the number of nodes that the indexer
 * has to work with.  In fact, this was the limiting factor in the past,
 * but I recently (at the request of Motorola) made icode_buf (which is
 * where the indexer gets its nodes) really big.
 *
 *		Added by Kevin, 11-6-92
 */

#define MAX_INDEX_BLOCK_SIZE 63000

/*
 * Size of a choice point for all implementations.
 */

#define chpt_SIZE 4


#ifdef arch_m68k

/*
 * Sun choice point structure
 *
 *	+-----------------------+
 *	|       Prev B          |	+12
 *	+-----------------------+
 *	|       Failure Point   | 	+8
 *	+-----------------------+
 *	|       SPB             |	+4
 *	+-----------------------+
 *	|       HB              | <-- B	+0
 *	+-----------------------+
 *
 * Note:
 * 	The movem instruction which is used for creating an instruction dictated
 *	the register layout be different than that previously designed for
 *	the 88k and later moved to the 80386.
 */

#define chpt_HB(b)		(* (((PWord **) (b)) ))
#define chpt_SPB(b)		(* (((PWord **) (b)) +1))
#define chpt_NextClause(b)	(* (((Code **) (b)) + 2))
#define chpt_B(b)		(* (((PWord **) (b)) +3))


#else /* arch_m68k */

/*
 * m88k, i386, vax, Portable, and Sparc choice point structure
 *
 *	+-----------------------+
 *	|       Prev B          |	+12
 *	+-----------------------+
 *	|       SPB             |	+8
 *	+-----------------------+
 *	|       HB              | 	+4
 *	+-----------------------+
 *	|       Failure Point   | <-- B	+0
 *	+-----------------------+
 */

#define chpt_NextClause(b) (* (Code **) (b))
#define chpt_HB(b) (* (((PWord **) (b)) + 1))
#define chpt_SPB(b) (* (((PWord **) (b)) + 2))
#define chpt_B(b) (* (((PWord **) (b)) + 3))

#endif /* arch_m68k */

#define chpt_COMPACTED(b) (((long) chpt_SPB(b)) & 1)


/*
 * GCMAGIC, GCMAGICVal, and GCMASK
 *
 * GCMAGIC (in combination with GCMASK if defined) is the opcode which
 * we expect immediately after a call.  GCMAGICVal gives the mask/argument
 * combination associated with the magic value and used by the garbage
 * compactor.
 *
 * If GCMASK is defined, we expect the magic value to be obtained from
 * two successive longwords.  This is the format used by the 88k and
 * Sparc since the continuation pointer is set so that the GCMAGIC instruction
 * is never executed (and thus we can put as much stuff in the code stream
 * as we want).
 *
 * ---------
 *
 * Retry / Trust chains in indexing blocks
 *
 * i386: 
 *      Size    What
 *      ----    ----
 *      1       mov eax,
 *      4               wm_retry or wm_trust
 *      2       call eax
 *      1       nop
 *      4       pointer to start of clause code or pointer to something else
 *              in this block
 *       
 * m68k: 
 *      2       nop 
 *      2       jsr 
 *      4       wm_retry or wm_trust 
 *      4       pointer to start of clause code or pointer to something else 
 *              in this block 
 * 
 * m88k: 
 *      4       bsr.n   wm_trustN or wm_trust_uN 
 *      4       addu    E, SPB, 0 
 *      4       br.n    offset to the start of clause code or offset to
 *                      something else in this block
 *      4       addu    SP, E, 0 
 *
 * vax:
 *	2	tstl	r0		(this is a nop)
 *	1	jsb
 *	1		ABSOLUTEADDRESSING MODE
 *	4	wm_retry or wm_trust
 *	4	pointer to start of clause code or pointer to something else
 *
 * Sparc:
 *	4	call	wm_trustN or wm_trust_uN
 *	4	add	0, SPB, E
 *	4	call	offset to the start of clause code or pointer to
 *			something else in this block
 *	4	add	0, E, SP
 *
 *              
 *
 * The i386, m68k, and vax schemes are close enough so that identical code may 
 * be used.  The m88k scheme will require somewhat different code due to 
 * the fact that the instruction sequence is one long word longer and due
 * to the fact that an offset instead of an absolute address is used. 
 *
 * IDXPATCH_SIZE is the size of one of these above segments in long words.
 * IDXPATCH_TARGET(p) gives the address of the clause code or the address
 *	of a word in the indexing block when supplied with one of these
 *	index patches.  The value will be given as a (long *).
 *
 * The other values defined here are used when generating indexing (index.c).
 * Units for the four following values are long words.
 *
 * TBENTSIZE is the size of a table entry (binary search tree entry or other
 * 	mechanism used to implement switch_on_const and switch_on_struct).
 * TBOVERHEAD is the overhead required for such a table.
 * TRYOVERHEAD is the additional space required by a try instruction
 * RETRYSIZE is the size of a retry or trust instruction.  This will be
 * 	the same as IDXPATCH_SIZE above.
 */

#define RETRYSIZE IDXPATCH_SIZE


#ifdef	arch_i386		/* i386 */
/* This is the op code for movl %eax, immediate */
#define GCMAGIC         ((char)0xb8)
/* And the immediate is a 32 bit value after the return address */
#define GCMAGICVal(ra)  *((long *)((Code *)ra+1))

#define IDXPATCH_SIZE 3
#define IDXPATCH_TARGET(p) *(((long **) p) + 2)
#define TBENTSIZE	2
#define TBOVERHEAD	3
#define TRYOVERHEAD	0
#endif	/* arch_i386 */


#ifdef	arch_m68k		/* m68k */
#ifdef MacOS
/* The MPW Assembler doesn't like evaluating expressions as immediate
   values, so I'll just do a move.w Addr, D0 to get the same effect.
   In this case, MPW Assembler only lays down 16 bits for the address,
   and, as long as only 16 bits are laid down, Raman has assured me
   that it will work correctly  (This clears me of any blame. :-) )
*/
#define GCMAGIC		0x3038
#else /* MacOS */
#define GCMAGIC		0x303c	/* opcode for move.w	#word_offset, D0 */
#endif /* MacOS */
#define GCMAGICVal(ra)  *((short *)((Code *)(ra)+1))

#define IDXPATCH_SIZE	3
#define IDXPATCH_TARGET(p) *(((long **) p) + 2)
#define TBENTSIZE	2
#define TBOVERHEAD	4
#define TRYOVERHEAD	0	
#endif	/* arch_m68k */


#ifdef	arch_vax		/* VAX */
#define GCMAGIC		0xb5	/* opcode for tstw */
#define GCMAGICVal(ra)	*((short *)((Code *)(ra)+2))

#define IDXPATCH_SIZE	3
#define IDXPATCH_TARGET(p) *(((long **) p) + 2)
#define TBENTSIZE	2
#define TBOVERHEAD	2
#define TRYOVERHEAD	0
#endif	/* arch_vax */

#ifdef	arch_m88k		/* m88k */
#define GCMASK          0xffe00000
#define GCMAGIC         0x60000000

#define OFF26MASK	0x03ffffff
#define OFF26SB		0x02000000
#define OFF26(i) (((i) & OFF26SB) ? ((i) | ~OFF26MASK) : ((i) & OFF26MASK))

#define IDXPATCH_SIZE	4
#define IDXPATCH_TARGET(p) (((long *) p) + 2 + OFF26(*(((long *) p) + 2)))

#define TBENTSIZE	2
#define TBOVERHEAD	2
#define TRYOVERHEAD	0
#endif	/* arch_m88k */


#ifdef arch_sparc		/* SPARC */
#define GCMASK		0xffc00000
#define GCMAGIC		0x00000000

#define IDXPATCH_SIZE	4
#define IDXPATCH_TARGET(p) ((long *)(((char *) p) + 8 + (*(((long *) p) + 2)<<2)))

#define TBENTSIZE	2
#define	TBOVERHEAD	2
#define TRYOVERHEAD	0
#endif /* arch_sparc */


#ifdef Portable			/* Portable */
#include "wamops.h"

#define GCMAGIC		abinst(W_GCMAGIC)
#define GCMAGICVal(ra)  *((long *)((Code *)(ra)+1))

#define IDXPATCH_SIZE	2
#define IDXPATCH_TARGET(p) *((long **)p + 1)
#define TBENTSIZE	2
#define TBOVERHEAD	2
#define TRYOVERHEAD	0
#endif /* Portable */

#endif /* _CHPT_H_INCLUDED_ */
