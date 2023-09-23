/*===================================================================*
 |		hztypes.h
 |	Copyright (c) 1987-95 Applied Logic Systems, Inc.
 |
 |		Low-level prolog types:
 |			-- Horizontal tagging; generally used in
 |				portable,i386,m68k,sparc,....
 |
 | This file contains all machine dependent manipulation of the machine
 | level Prolog types (what the WAM uses)
 |
 | See the "Machine Dependent Files" section of the implementation
 | documentation for a philosophy of this file.
 |
 | Author: Kevin A. Buettner
 | Created: 2/17/87 
 | 03/10/89	K.Hughes -- Consolidated all low-level Prolog type/type
 |			manipulation information to mtypes.h; kept in
 |			machine-specific directories
 | 03/01/95 K.Bowen -- Created hztypes.h in generic from mtypes.h in
 |			portable directory; all "horizontal" tagging implementations
 |			now include this, via: the old mtypes.h includes this, and
 |			contains some specific info (e.g., max integer size)
 *===================================================================*/

/*@[3.2]@----------------------------------------------------------
 | The low level types use the bottom two bits for primary tag 
 | differentiation.  The following give the bit patterns for the basic 
 | types, as well as the mask needed to get just these bits left in 
 | the integer being examined.
 *-----------------------------------------------------------------*/

#include "wd_size.h"

#define MTP_TAGMASK	0x3
#define MTP_TAG(o) (((PWord)(o)) & MTP_TAGMASK)

#define MTP_UNBOUND	0
#define MTP_STRUCT	1
#define MTP_LIST	2
#define MTP_CONST	3

/*-----------------------------------------------------------------------
 | In horizontal tagging, the constant type includes 4 types, being
 | integers, symbols, UIAs and fences. The lower 4 bits are used to 
 | differentiate these types. The bottom two bits will be from MTP_CONST 
 | above, while the next two give the actual differentiation.
 | 
 | The following mask gives the bits which give this tag, while the rest
 | gives the bit patterns for the 4 bits, including the MTP_CONST bits.
 | 
 | 	MTP_INT		(0 << 2) | MTP_CONST
 | 	MTP_SYM		(1 << 2) | MTP_CONST
 | 	MTP_FENCE	(2 << 2) | MTP_CONST
 | 	MTP_UIA		(3 << 2) | MTP_CONST
 *-----------------------------------------------------------------------*/

#define MTP_CONSTMASK	0xf
#define MTP_CONSTSHIFT	4
#define MTP_CONSTTAG(c) (((PWord)(c)) & MTP_CONSTMASK)

#define MTP_INT		    3
#define MTP_SYM		    7
#define MTP_FENCE	    0xb
#define MTP_UIA		    0xf

/*-----------------------------------------------------------------------
 | Define sign bit position in word. ANDing MTP_SIGNBIT with a word will 
 | produce a non-zero result if the word is negative. MTP_SIGNEXT gives a 
 | pattern to OR onto the corrected integer to extend this sign to the 
 | correct position in a full word integer representation.
 *-----------------------------------------------------------------------*/

#ifdef __LP64__
#define MTP_SIGNBIT	0x8000000000000000L
#define MTP_SIGNEXT	0xf000000000000000L
#else
#define MTP_SIGNBIT	0x80000000L
#define MTP_SIGNEXT	0xf0000000L
#endif
/*-----------------------------------------------------------------------
 | MBIAS and MUNBIAS are required on the 88k.  When possible, these macros
 | are not used except in the 88k mtypes file.  There are several places in
 | the source where BIAS factors need to be explicitly introduced.  MBIAS and
 | MUNBIAS on the non-88k archs are simply identity operations.
 *-----------------------------------------------------------------------*/

#define MBIAS(x) (x)
#define MUNBIAS(x) (x)

/*-----------------------------------------------------------------------
 | The following provide ways of creating the various low level types.
 *-----------------------------------------------------------------------*/

#ifdef Portable
	/*  Using these makes sio.o bomb on sparc (probably atrans is messing up):*/
#define MMK_INT(i)       ((((unsigned long)(i))<<MTP_CONSTSHIFT)|MTP_INT)
#define MMK_SYM(i)       ((((PWord)(i))<<MTP_CONSTSHIFT)|MTP_SYM)
#define MMK_FUNCTOR(s,a) (MMK_SYM(s) | (((PWord)(a))<<FCTRSHIFT))
#else
#define MMK_INT(i) (((i)<<MTP_CONSTSHIFT)|MTP_INT)
#define MMK_SYM(i) (((i)<<MTP_CONSTSHIFT)|MTP_SYM)
#define MMK_FUNCTOR(s,a) (MMK_SYM(s) | ((a)<<FCTRSHIFT))
#endif

#define MMK_FENCE(n)     ((((PWord)(n))<<MTP_CONSTSHIFT)|MTP_FENCE)
#define MMK_UIA(v)       ((((PWord)(v))<<MTP_CONSTSHIFT)|MTP_UIA)
#define MMK_LIST(v)      (((PWord)(v)) | MTP_LIST)
#define MMK_STRUCTURE(v) (((PWord)(v)) | MTP_STRUCT)
#define MMK_VAR(v)       ((PWord)(v))
#define MMK_UIAVAL(v)    ((char *)(v) - (char *)wm_heapbase )

/*-----------------------------------------------------------------------
 | The following provide ways of getting information from the basic types.
 | The following either return the value required (integer, symbol index, 
 | UIA) or a pointer to a heap object (structure or list)
 *-----------------------------------------------------------------------*/

#define MFUNCTOR_TOKID(f) ((((PWord)(f)) & FCTRTOKMASK) >> MTP_CONSTSHIFT)
#define MFUNCTOR_ARITY(f) ((int)((((unsigned long)(f)) >> FCTRSHIFT) & FCTRARTYMASK))
#define MLISTADDR(l)	  ((PWord *)(((PWord)(l)) & ~(PWord)MTP_TAGMASK))
#define MSTRUCTADDR(s)	  ((PWord *)(((PWord)(s)) & ~(PWord)MTP_TAGMASK))
#define MFUNCTOR(s)	      (*(PWord *)(s))
#define MSUBTERMN(s,n)	  (*((PWord *)(s) + (int)(n)))
#define MVAR(v)		      ((PWord)(v))
#define MUIA(f)		      (((PWord)(f)) >> MTP_CONSTSHIFT)
#define MSYMBOL(f)	      (((PWord)(f)) >> MTP_CONSTSHIFT)
#define MINTEGER(i)	      ((((PWord)(i)) & MTP_SIGNBIT) ? \
			   (MTP_SIGNEXT | (((PWord)(i)) >> MTP_CONSTSHIFT)) : \
			   ((((PWord)(i)) >> MTP_CONSTSHIFT) & ~MTP_SIGNEXT))
/* #define MUIAADDR(u)	      (wm_heapbase + MUIA(u)) */
#define MUIAADDR(u)	((PWord *) (((char *) wm_heapbase) + (MUIA(u))))
#define MLIST_CAR(l)      (*(PWord *)(l))
#define MLIST_CDR(l)      (*(((PWord *)(l))+1))

/*-----------------------------------------------------------------------
 | Some simple tag checks.
 *-----------------------------------------------------------------------*/

#define M_ISVAR(v)		(!MTP_TAG(v))
#define M_ISLIST(v)     (MTP_TAG(v) == MTP_LIST)
#define M_ISSTRUCT(v)   (MTP_TAG(v) == MTP_STRUCT)
#define M_ISSYM(v)		(MTP_CONSTTAG(v) == MTP_SYM)
#define M_ISUIA(v)		(MTP_CONSTTAG(v) == MTP_UIA)

/*-----------------------------------------------------------------------
 | Some misc macros.
 *-----------------------------------------------------------------------*/

#define M_VARVAL(v)	      (*(PWord *)(v))	/* Get the value of a variable */
#define MFENCE_VAL(v)     (((PWord) (v)) >> MTP_CONSTSHIFT)

#define M_FIRSTUIAWORD(v) ((PWord *)((char *)wm_heapbase + (long)(v))+1)
#define M_UIASIZE(v)      ((int)(MFENCE_VAL(*((PWord *)((char *)wm_heapbase + \
                                          (int)(v)))) - 1) * sizeof(PWord))

#ifdef SUBTYPES

/*-----------------------------------------------------------------------
 | Sub-UIA types.
 *-----------------------------------------------------------------------*/

	/* Full long integers  */
#define SUT_INT		    1

#endif /* SUBTYPES */
