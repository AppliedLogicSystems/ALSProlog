/*=======================================================================*
 |			bmisc.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems
 |
 |			-- Misc Prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | Revision History: (fixes, not addition of new builtins)
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 | 10/26/94 - C. Houpt -- Added char* casts for standard library sprintf call.
 *=======================================================================*/
#include "defs.h"
#include "wintcode.h"
#include "compile.h"		/* for value of NAREGS */

#include <stdio.h>
#include <time.h>

static	unsigned long hashN	( PWord, int, int );
static	void	als_gensym	( UCHAR *, UCHAR * );

#ifdef CMeta

static int
termcmp(PWord v1, int t1, PWord v2, int t2)
{
    int   r;
    PWord av1, av2;
    int   at1, at2;
    PWord f1, f2;
    int   a1, a2, i;
    double dv1, dv2;
    const char *sym1 = NULL, *sym2 = NULL;

    /*
     * Do some conversion to simplify comparison.
     * - Convert double structures to a fake double-type.
     * - Convert lists to '.'/2 structures.
     * - Extract string pointer from symbols and UIAs
     */
 
    switch (t1) {
    case WTP_STRUCTURE:
    	w_get_functor(&f1, v1);
	w_get_arity(&a1, v1);
	if (f1 == TK_DDOUBLE && a1 == 4) {
	    for (i = 0; i < 4; i++) {
		w_get_argn(&av1, &at1, v1, i + 1);
		*(((short *) &dv1) + i) = av1;
	    }
	    t1 = WTP_DOUBLE;
	}
	break;
    case WTP_LIST: 
	f1 = TK_DOT;
	a1 = 2;
	t1 = WTP_STRUCTURE;
	v1 -= sizeof (PWord);	/* see list as a structure */
	break;
    case WTP_SYMBOL:
    	sym1 = (char *)TOKNAME(v1);
    	break;
    case WTP_UIA:
    	t1 = WTP_SYMBOL;
    	sym1 = (char *)M_FIRSTUIAWORD(v1);
    	break;
    }

    switch (t2) {
    case WTP_STRUCTURE:
    	w_get_functor(&f2, v2);
	w_get_arity(&a2, v2);
	if (f2 == TK_DDOUBLE && a2 == 4) {
	    for (i = 0; i < 4; i++) {
		w_get_argn(&av2, &at2, v2, i + 1);
		*(((short *) &dv2) + i) = av2;
	    }
	    t2 = WTP_DOUBLE;
	}
	break;
    case WTP_LIST: 
	f2 = TK_DOT;
	a2 = 2;
	t2 = WTP_STRUCTURE;
	v2 -= sizeof (PWord);
	break;
    case WTP_SYMBOL:
    	sym2 = (char *)TOKNAME(v2);
    	break;
    case WTP_UIA:
    	t2 = WTP_SYMBOL;
    	sym2 = (char *)M_FIRSTUIAWORD(v2);
    	break;
    }
    
    /* The big compare switch */
    
    switch (t1) {
    case WTP_UNBOUND:
    	switch (t2) {
    	case WTP_UNBOUND:
	    if (v1 < v2) return -1;
	    else if (v1 > v2) return 1;
	    else return 0;
    	    break;
    	default:
    	    return -1; break;
    	}
    case WTP_DOUBLE:
    	switch (t2) {
    	case WTP_UNBOUND:
    	    return 1; break;
    	case WTP_DOUBLE:
	    if (dv1 < dv2) return -1;
	    else if (dv1 > dv2) return 1;
	    else return 0;
	    break;
	default:
	    return -1; break;
    	}
        break;
    case WTP_INTEGER:
    	switch (t2) {
    	case WTP_UNBOUND:
	case WTP_DOUBLE:
    	    return 1; break;
    	case WTP_INTEGER:
	    if (v1 < v2) return -1;
	    else if (v1 > v2) return 1;
	    else return 0;
	    break;
	default:
	    return -1; break;
    	}
        break;
    case WTP_SYMBOL:
    	switch (t2) {
    	case WTP_UNBOUND:
	case WTP_DOUBLE:
    	case WTP_INTEGER:
    	    return 1; break;
    	case WTP_SYMBOL:
    	    return strcmp(sym1, sym2);
    	    break;
	default:
	    return -1; break;
    	}
        break;
    case WTP_STRUCTURE:
    	switch (t2) {
    	case WTP_UNBOUND:
	case WTP_DOUBLE:
    	case WTP_INTEGER:
    	case WTP_SYMBOL:
    	    return 1; break;
    	case WTP_STRUCTURE:
	    if (a1 < a2)
		return -1;
	    else if (a2 < a1)
		return 1;
	    if ( (r = strcmp((char *)TOKNAME(f1), (char *)TOKNAME(f2))) )
		return r;
	    for (i = 1; i <= a1; i++) {
		w_get_argn(&av1, &at1, v1, i);
		w_get_argn(&av2, &at2, v2, i);
		if ( (r = termcmp(av1, at1, av2, at2)) )
		    return r;
	    }
	    return 0;
    	    break;
	default:
	    return -1; break;
    	}
        break;
    default:
    	fatal_error(FE_IN_TERMCMP, 0);
	return 0;	/* this statement not reached */
	break;
    }
}

int
pbi_compare()
{				/* compare(Rel,Term1,Term2)   */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    int   res;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    res = termcmp(v2, t2, v3, t3);

    if (w_unify(v1, t1,
		(PWord) ((res < 0) ? TK_LESS : ((res > 0) ? TK_GRT : TK_EQ)), WTP_SYMBOL))
	SUCCEED;
    else
	FAIL;
}

int
wm_identical(v1, t1, v2, t2)
    PWord v1;
    int   t1;
    PWord v2;
    int   t2;
{
    PWord argval1, argval2;
    int   argtype1, argtype2;
    PWord functor1, functor2;
    int   arity1, arity2;
    int   i;

    if (t1 == t2)
	switch (t1) {
	    case WTP_UNBOUND:
	    case WTP_INTEGER:
	    case WTP_SYMBOL:
		if (v1 == v2)
		    return (1);
		else
		    return (0);
		break;
	    case WTP_LIST:
		w_get_car(&argval1, &argtype1, v1);
		w_get_car(&argval2, &argtype2, v2);
		if (!wm_identical(argval1, argtype1, argval2, argtype2))
		    return (0);
		w_get_cdr(&argval1, &argtype1, v1);
		w_get_cdr(&argval2, &argtype2, v2);
		if (!wm_identical(argval1, argtype1, argval2, argtype2))
		    return (0);
		return (1);
		break;
	    case WTP_STRUCTURE:
		w_get_functor(&functor1, v1);
		w_get_functor(&functor2, v2);
		w_get_arity(&arity1, v1);
		w_get_arity(&arity2, v2);
		if ((functor1 == functor2) && (arity1 == arity2)) {
		    for (i = 1; i <= arity1; i++) {
			w_get_argn(&argval1, &argtype1, v1, i);
			w_get_argn(&argval2, &argtype2, v2, i);
			if (!wm_identical(argval1, argtype1, argval2, argtype2))
			    return (0);
		    }
		    return (1);
		}
		else
		    return (0);
		break;
	    case WTP_UIA:
		if (strcmp((char *) M_FIRSTUIAWORD(v1), 
			   (char *) M_FIRSTUIAWORD(v2)) == 0)
		    return (1);
		else
		    return (0);
		break;
	    default:
		return (0);
		break;
	}
    else
	/*
	 * Types of objects are different.
	 */
	switch (t1) {
	    case WTP_SYMBOL:
		if (t2 == WTP_UIA) {
		    if (strcmp((char *)TOKNAME(v1),
			(char *) M_FIRSTUIAWORD(v2)) == 0)
			return (1);
		}
		return (0);
		break;
	    case WTP_UIA:
		if (t2 == WTP_SYMBOL) {
		    if (strcmp((char *) M_FIRSTUIAWORD(v1), 
			(char *)TOKNAME(v2)) == 0)
			return (1);
		}
		return (0);
		break;
	    default:
		return (0);
		break;
	}
}

#endif /* CMeta */

#ifdef HASH
static unsigned long
hashN(v, t, d)
    PWord v;
    int   t;
    int   d;
{
    if (d <= 0)
	return 0;
    else
	switch (t) {
	    case WTP_LIST:{
		    PWord vh, vt;
		    int   th, tt;

		    w_get_car(&vh, &th, v);
		    w_get_cdr(&vt, &tt, v);
		    return (hashN(vh, th, d - 1) ^ hashN(vt, tt, d - 1));
		}
	    case WTP_STRUCTURE:{
		    PWord functor, va;
		    int   i, arity, ta;
		    unsigned long  acc;

		    w_get_functor(&functor, v);
		    w_get_arity(&arity, v);
		    acc = ((long) functor) * arity * d;
		    if (d > 1) {
			for (i = 1; i <= arity; i++) {
			    w_get_argn(&va, &ta, v, i);
			    acc ^= hashN(va, ta, d - 1);
			}
		    }
		    return acc;
		}
	    case WTP_SYMBOL:{
		    register UCHAR *s = TOKNAME(v);
		    int   shift;
		    unsigned long  acc;

		    shift = (*s & 0x0f) + d;
		    acc = *s;
		    if (acc) {
			acc ^= (*(s + 1));
		    }
		    for (; *s; shift += 4) {
			acc = acc + ((*s++) << shift);
			if (shift >= 17)
			    shift -= 17;
		    }
		    return acc;
		}
	    case WTP_INTEGER:
		return (unsigned long) (d << 5) + v;
	    case WTP_UIA:{
		    register UCHAR *s = 
			(UCHAR *) M_FIRSTUIAWORD(v);
		    int   shift;
		    long  acc;

		    shift = (*s & 0x0f) + d;
		    acc = *s;
		    if (acc) {
			acc ^= (*(s + 1));
		    }
		    for (; *s; shift += 4) {
			acc = acc + ((*s++) << shift);
			if (shift >= 17)
			    shift -= 17;
		    }
		    return acc;
		}
#ifdef	DoubleType
	    case WTP_DOUBLE:{
		    double dbl;

		    w_get_double(&dbl, v);
		    return d + (((long *)  &dbl)[0] ^ ((long *) &dbl)[1]);
		}
#endif /* DoubleType */
	    default:
		return 0;
	}
}

int
pbi_hashN()
{				/* hashN(Term,N,Depth,HashVal) */
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (t2 != WTP_INTEGER || t3 != WTP_INTEGER) {
	FAIL;
    }
    else {
	if (w_unify(v4, t4, 
		    (PWord) (hashN(v1, t1, (int) v3) % v2) + 1, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
}
#endif /* HASH */

#ifdef GENSYM
static long gensym_start_time = 0;
static long gensym_counter = 0;

static void
als_gensym(buffer, prefix)
    UCHAR *buffer, *prefix;
{
    if (!gensym_start_time) gensym_start_time = time(NULL);

    sprintf((char *)buffer, "%c%s_%ld_%ld",
	    Generated_Symbol_Starting_Character,
	    (char *)prefix,
	    gensym_start_time,
	    gensym_counter++);
}


int
pbi_gensym()
{
    PWord v1, v2, vr;
    int   t1, t2, tr;
    UCHAR *prefix;
    UCHAR *buffer;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (!getstring(&prefix, v1, t1))
	FAIL;

    buffer = (UCHAR *) (wm_H + 1);
    als_gensym(buffer, prefix);

    w_mk_uia_in_place(&vr, &tr, buffer);

    if (w_unify(v2, t2, vr, tr))
	SUCCEED;
    else
	FAIL;
}

int
pbi_isgensym()
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *prefix, *testsym;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (getstring(&testsym, v2, t2) && getstring(&prefix, v1, t1)) {
	register UCHAR *p, *t;

	p = prefix;
	t = testsym;
	if (*t++ == Generated_Symbol_Starting_Character) {
	    while (*p) {
		if (*p++ != *t++)
		    FAIL;
	    }
	    SUCCEED;
	}
    }

    FAIL;
}
#endif /* GENSYM */

#ifdef PRIM_DBG

/*
 * ptermaddr/1
 *
 * Called from prolog with a single argument.  The type and address (or value)
 * is printed out.  This is a primitive debugging predicate.
 *
 */
static const char *typestrings[] =
{
    "unbound",
    "list",
    "structure",
    "symbol",
    "integer",
    "uia",
    "double"
};

int
pbi_ptermaddr()
{
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);
    if (t1 == WTP_UIA)
	v1 = (PWord) (M_FIRSTUIAWORD(v1) - 1);
    PI_printf("type: %s\taddr/val: %#x\n", typestrings[t1], v1);
    SUCCEED;
}

/*
 *
 * Called from prolog to print out the tail/chpt stack.  This is a primitive
 * debugging predicate.
 */

int
pbi_traildump()
{
    PWord *tr, *b;

    for (tr = wm_TR, b = wm_B; tr < wm_trailbase;) {
	if (tr == b) {
	    printf("%8lx: Fail:%8lx  HB:%8lx  SPB:%8lx  B:%8lx\n",
		   (long) b,
		   (long) chpt_NextClause(b),
		   (long) chpt_HB(b),
		   (long) chpt_SPB(b),
		   (long) chpt_B(b));
	    tr += chpt_SIZE;
	    b = chpt_B(b);
	}
	else {
	    printf("%8lx: %lx\n", (long) tr, *tr);
	    tr++;
	}
    }
    SUCCEED;
}

/*---------------------------------------------------------------------
 * frame_info/2
 *
 *	The first argument is a count of the number of frames to go back.
 *	The second argument is unified with the goal (including the module)
 *	at the Nth frame back where N was the integer value of the first
 *	argument.
 *
 *	frame_info/2 should be used for informational or debugging
 *	purposes only.  It is not possible to always obtain all of the
 *	arguments in the frame.  This happens for two reasons:
 *		1) Certain platforms pass some of the arguments in
 *		registers.  If the register argument is used early
 *		in the clause, it may never get placed on the stack.
 *		2) Even if an argument is placed on the stack at the
 *		time of the call, there is no guarantee that it is
 *		safe to refer to it at some later point.  A gc
 *		may have moved a pointer argument without updating
 *		the pointer if that argument is no longer needed
 *		to properly complete execution of the clause. Note
 *		that if a constant appears in one of these positions,
 *		then that constant *is* valid -- at least on platforms
 *		where the arguments are not passed in registers.
 *	In cases where we are not certain that the argument is valid,
 *	we use a question mark in the place of the argument to indicate
 *	this condition.
 *-------------------------------------------------------------------*/

int
pbi_frame_info()	/* frame_info(Count,Goal) */
{
    PWord v1, v2, vg, va, vr;
    int   t1, t2, tg, ta, tr;
    PWord *e;
    long  *ca, amask;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER)
	FAIL;
    
    for (e = wm_SP, ca=0; e && v1 > 0; v1--)
	e = w_frame_info(e,&ca,&amask);
    
    if (e && ca) {
	PWord pn, mn;
	int arity;
	pn = MFUNCTOR_TOKID(w_nametable[procIdx(ca)]->tokid_arity);
	arity = MFUNCTOR_ARITY(w_nametable[procIdx(ca)]->tokid_arity);
	mn = w_nametable[procIdx(ca)]->modid;
	if (arity == 0) {
	    vg = pn;
	    tg = WTP_SYMBOL;
	}
	else {
	    int i;
	    PWord *b;
	    w_mk_term(&vg,&tg,pn,arity);

	    for (b = wm_B; 
		 b && (PWord *) MUNBIAS((long) chpt_SPB(b) & ~1) < e;
		 b = chpt_B(b))
		 ;
	    /* If choice point points at this frame, then all args are good */
	    if ((PWord *) MUNBIAS((long) chpt_SPB(b) & ~1) == e)
		amask = ~0;

	    for (i=1; i <= arity; i++) {
		if ((amask & 1) || arity > 32
#ifdef MTP_CONST
		    || MTP_CONSTTAG(*(e+i+1)) == MTP_INT
		    || MTP_CONSTTAG(*(e+i+1)) == MTP_SYM
#else	/* MTP_CONST */
		    || MTP_TAG(*(e+i+1)) == MTP_INT
		    || MTP_TAG(*(e+i+1)) == MTP_SYM
#endif  /* MTP_CONST */
		   ) {
		    w_get(&va, &ta, *(e+i+1));
		    if (ta != WTP_UNBOUND) /* install if not variable */
			w_install_argn(vg, i, va, ta);
		    else {	/* unify if var as it might live on stack */
			PWord vv;
			int tv;
			w_install_unbound_argn(vg,i);
			w_get_argn(&vv,&tv,vg,i);
			(void) w_unify(va,ta,vv,tv);
		    }
		}
		else
		    w_install_argn(vg, i, char_to_tok('?'), WTP_SYMBOL);
		amask >>= 1;
	    }
	}

	w_mk_term(&vr, &tr, TK_COLON, 2);
	w_install_argn(vr,1,mn,WTP_SYMBOL);
	w_install_argn(vr,2,vg,tg);
    }
    else
	FAIL;
    
    if (w_unify(v2,t2, vr,tr))
	SUCCEED;
    else
	FAIL;
}

#endif /* PRIM_DBG */
