/*
 * mtypes.h			-- low level prolog types
 *	Copyright (c) 1987-1993 Applied Logic Systems
 *
 * Author: Kevin A. Buettner
 * Created: 2/17/87
 * Revision History:
 *	Revised: 3/30/93,	kev	-- m88k --> HPPA modifications
 */

/*
 * On the 88k all of the types are represented via the most significant
 * six bits.
 */

#define MTP_TAGMASK 0xfc000000
#define MTP_TAG(o) ((o) & MTP_TAGMASK)


#define MTP_UNBOUND 0
#define MTP_STRUCT 0x84000000
#define MTP_LIST  0x88000000

/*
 * On other implementations, MTP_CONST indicates one of the following objects;
 * on the 88k, these objects are obtained in the same manner as structures
 * and lists (above)
 */


#define MTP_INT 0xa0000000
#define MTP_SYM 0xc0000000
#define MTP_FENCE 0xec000000
#define MTP_UIA 0xd0000000
#define MTP_DOUBLE 0xb0000000

/*
 * The ground bit
 */

#define MTP_BGND  0x80000000


#define MTP_VALMASK 0x03ffffff
#define MTP_SIGNBIT 0x02000000
#define MTP_SIGNEXT 0xfe000000

/*
 * MT_BIAS is the bias factor used with addresses.  Due to some unfortunate
 * architectural constraints of the 88k, it was necessary to adjust all prolog
 * addresses so that they point 32768 prior to where they really want to
 * point.  MT_BIAS below is used with longword pointers to handle this
 * adjustment so that the rest of the C code need not worry about it.  MT_BIAS
 * should be only appear in this file alone.  If any other code needs to worry
 * about it, then it is not written correctly.
 *
 * MBIAS is used to convert a C pointer to one which points 32k prior to where
 * it wants to.  It is also ready to accept a Prolog tag.
 *
 * MUNBIAS is used to take a prolog object (any address type) and make it
 * point to where it is really supposed to point.
 */

#define MT_BIAS 0
#define MBIAS(v) (long) (((long *) (v)))
#define MUNBIAS(v) ((((long) (v)) & ~MTP_TAGMASK))


/*
 * Macros for creating objects of the low level types
 */

#define MMK_INT(i) (((i)&MTP_VALMASK)|MTP_INT)
#define MMK_SYM(i) ((i)|MTP_SYM)
#define MMK_FUNCTOR(s,a) (MMK_SYM(s) | ((a)<<16))
#define MMK_FENCE(n) ((n)|MTP_FENCE)
#define MMK_UIA(v) (MBIAS(v)|MTP_UIA)
#define MMK_LIST(v) (MBIAS(v) | MTP_LIST)
#define MMK_STRUCTURE(v) (MBIAS(v) | MTP_STRUCT)
#define MMK_VAR(v) MBIAS(v)
#define MMK_UIAVAL(v) (v)

#define MMK_DOUBLE(v) (MBIAS(v) | MTP_DOUBLE)

/*
 * The following macros provide ways of getting information from the basic types.
 *
 * They either give the value required (integer, symbol index, UIA) or
 * pointer to a heap object (structure or list)
 */

#define MFUNCTOR_TOKID(f) ((f) & 0xffff)
#define MFUNCTOR_ARITY(f) (((f) >> 16) & 0x3ff)
#define MLISTADDR(l)	((long *) MUNBIAS(l))
#define MSTRUCTADDR(s)	((long *) MUNBIAS(s))
#define MFUNCTOR(s) 	((long) (*(s)))
#define MSUBTERMN(s,n)	((long) (*((s) + (n))))
#define MVAR(v)		MUNBIAS(v)
#define MUIA(f)		((long *) MUNBIAS(f))
#define MSYMBOL(f)	MFUNCTOR_TOKID(f)
#define MINTEGER(o) (((o)&MTP_SIGNBIT) ? ((o)|MTP_SIGNEXT) : ((o)&MTP_VALMASK))
#define MUIAADDR(u)	MUIA(u)
#define MDOUBLEADDR(d)	((long *) MUNBIAS(d))
#define MOBJ_ADDR(o) ((o)&~MTP_TAGMASK)


/*
 * Some simple tag checks.
 */

#define M_ISVAR(v) (!((v) & MTP_TAGMASK))
#define M_ISSYM(v) (((v) & MTP_TAGMASK) == MTP_SYM)
#define M_ISUIA(v) (((v) & MTP_TAGMASK) == MTP_UIA)


/*
 * Some misc macros.
 */

#define M_VARVAL(v) (* (long *) MUNBIAS(v))	/* Get value of a variable */
#define MFENCE_VAL(o) ((o)&~MTP_TAGMASK)	/* Get fence value */

#define M_FIRSTUIAWORD(v) (((long *) v) + 1)
#define M_UIASIZE(v)   ((int)(MFENCE_VAL(*((PWord *)v)) - 1) * sizeof(PWord)) 



/*
 * Maximum and Minimum Prolog integers (orignally from arith.h)
 */

#define MAXPROLOGINT 0x01ffffff		/* 26 bit integers */
#define MINPROLOGINT ((int) 0xfe000000)
