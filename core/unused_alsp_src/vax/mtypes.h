/*
 * mtypes.h			-- low level prolog types
 *
 *	Implementation-dependent
 *
 * This file should contain all machine dependent manipulation of the machine
 * level Prolog types (what the WAM uses), so that they are easy to find
 * and change if necessary.
 *
 * See the "Machine Dependent Files" section of the implementation
 * documentation for a philosophy of this file.
 *
 *	Copyright (c) 1987-91 Applied Logic Systems
 *
 * Author: Kevin A. Buettner
 * Created: 2/17/87
 * Revision History
 *		3/10/89	kmh	Consolidated all low-level Prolog type/type
 *				manipulation information to mtypes.h
 */

/*
The low level types use the bottom two bits for primary tag differentiation.
The following give the bit patterns for the basic types, as well as the mask
needed to get just these bits left in the integer being examined.
*/

#define MTP_TAGMASK	0x3
#define MTP_TAG(o) ((o) & MTP_TAGMASK)

#define MTP_UNBOUND	0
#define MTP_STRUCT	1
#define MTP_LIST	2
#define MTP_CONST	3

/*
In the current implementation, the constant type includes 4 types, being
integers, symbols, UIAs and fences. The lower 4 bits are used to differentiate
these types. The bottom two bits will be from MTP_CONST above, while the
next two give the actual differentiation.

The following mask gives the bits which give this tag, while the rest
gives the bit patterns for the 4 bits, including the MTP_CONST bits.

	MTP_INT		(0 << 2) | MTP_CONST
	MTP_SYM		(1 << 2) | MTP_CONST
	MTP_FENCE	(2 << 2) | MTP_CONST
	MTP_UIA		(3 << 2) | MTP_CONST
*/

#define MTP_CONSTMASK	0xf
#define MTP_CONSTSHIFT	4
#define MTP_CONSTTAG(c) ((c) & MTP_CONSTMASK)

#define MTP_INT		3
#define MTP_SYM		7
#define MTP_FENCE	0xb
#define MTP_UIA		0xf

/*
Define sign bit position in word. ANDing MTP_SIGNBIT with a word will produce
a non-zero result if the word is negative. MTP_SIGNEXT gives a pattern to
OR onto the corrected integer to extend this sign to the correct position
in a full word integer representation.
*/

#define MTP_SIGNBIT	0x80000000
#define MTP_SIGNEXT	0xf0000000

/*
 * MBIAS and MUNBIAS are required on the 88k.  When possible, these macros
 * are not used except in the 88k mtypes file.  There are several places in
 * the source where BIAS factors need to be explicitly introduced.  MBIAS and
 * MUNBIAS on the 68k are simply identity operations.
 */

#define MBIAS(x) (x)
#define MUNBIAS(x) (x)

/*
The following provide ways of creating the various low level types.
*/

#define MMK_INT(i) (((i)<<MTP_CONSTSHIFT)|MTP_INT)
#define MMK_SYM(i) (((i)<<MTP_CONSTSHIFT)|MTP_SYM)
#define MMK_FUNCTOR(s,a) (MMK_SYM(s) | ((a)<<24))
#define MMK_FENCE(n) (((n)<<MTP_CONSTSHIFT)|MTP_FENCE)
#define MMK_UIA(v) (((v)<<MTP_CONSTSHIFT)|MTP_UIA)
#define MMK_LIST(v) ((v) | MTP_LIST)
#define MMK_STRUCTURE(v) ((v) | MTP_STRUCT)
#define MMK_VAR(v) (v)
#define MMK_UIAVAL(v) (((char *) v) - ((char *) wm_heapbase))

/*
The following provide ways of getting information from the basic types.

The following either give the value required (integer, symbol index, UIA) or
a pointer to a heap object (structure or list)
*/

#define MFUNCTOR_TOKID(f) (((f) & 0xffffff) >> MTP_CONSTSHIFT)
#define MFUNCTOR_ARITY(f) (((f) >> 24) & 0xff)
#define MLISTADDR(l)	((PWord *)((l) & ~MTP_TAGMASK))
#define MSTRUCTADDR(s)	((PWord *)((s) & ~MTP_TAGMASK))
#define MFUNCTOR(s)	((PWord)(*(s)))
#define MSUBTERMN(s,n)	((PWord)(*((s) + (n))))
#define MVAR(v)		(v)
#define MUIA(f)		((f) >> MTP_CONSTSHIFT)
#define MSYMBOL(f)	((f) >> MTP_CONSTSHIFT)
#define MINTEGER(i)	(((i) & MTP_SIGNBIT) ? \
			   (MTP_SIGNEXT | ((i) >> MTP_CONSTSHIFT)) : \
			   (((i) >> MTP_CONSTSHIFT) & ~MTP_SIGNEXT))
#define MUIAADDR(u)	((PWord *) (((char *) wm_heapbase) + (MUIA(u))))

/*
Some simple tag checks.
*/

#define M_ISVAR(v)	(!((v) & MTP_TAGMASK))
#define M_ISSYM(v)	(((v) & MTP_CONSTMASK) == MTP_SYM)
#define M_ISUIA(v)	(((v) & MTP_CONSTMASK) == MTP_UIA)

/*
Some misc macros.
*/

#define M_VARVAL(v)	(*(PWord *)v)	/* Get the value of a variable */
#define MFENCE_VAL(v) (((PWord) (v)) >> MTP_CONSTSHIFT)

#define M_FIRSTUIAWORD(v)	((PWord *)((char *)wm_heapbase + (int)(v))+1)
#define M_UIASIZE(v)    ((int)(MFENCE_VAL(*((PWord *)((char *)wm_heapbase + \
                                          (int)(v)))) - 1) * sizeof(PWord))
 


/*
 * Maximum and Minimum Prolog integers (originally from arith.h)
 */

#define MAXPROLOGINT (PWord)0x07ffffff		/* 28 bit integers */
#define MINPROLOGINT (PWord)0xf8000000

/*
 * Arity which indicates that we have a big structure
 */

#define ESCAPE_ARITY 255
