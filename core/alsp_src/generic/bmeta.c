/*===================================================================*
 |		bmeta.c   
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-1993 by Applied Logic Systems
 |
 |		-- prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 *===================================================================*/
#include "defs.h"
#include "module.h"
#include "icodegen.h"

#ifdef CMeta

int
pbi_true()
{
    SUCCEED;
}

int
pbi_equal()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (w_unify(v1, t1, v2, t2))
	SUCCEED;
    else
	FAIL;
}

int
pbi_arg()
{
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord functor;
    int   arity;
    PWord argval;
    int   argtype;

    w_get_An(&v1, &t1, 1);	/* Get argument number  */
    w_get_An(&v2, &t2, 2);	/* Get structure or list */
    w_get_An(&v3, &t3, 3);	/* Get argument to unify with */

    if (t1 != WTP_INTEGER)
	FAIL;

    switch (t2) {
	case WTP_STRUCTURE:
	    w_get_functor(&functor, v2);
	    w_get_arity(&arity, v2);
	    if (v1 > arity || v1 < 1)
		FAIL;

	    w_get_argn(&argval, &argtype, v2, (int) v1);

	    break;

	case WTP_LIST:
	    if (v1 == 1)
		w_get_car(&argval, &argtype, v2);
	    else if (v1 == 2)
		w_get_cdr(&argval, &argtype, v2);
	    else
		FAIL;

	    break;

	default:
	    FAIL;
    }

    if (w_unify(argval, argtype, v3, t3))
	SUCCEED;
    else
	FAIL;
}

#ifndef BigStruct
#define w_get_argaddr(addr,s,argn,arity)  (addr = (PWord *)s + argn)
#else  /* BigStruct */
#define w_get_argaddr(addr,s,argn,arity)  \
	{if (arity < ESCAPE_ARITY) addr = (PWord *)s + argn; else addr=(PWord *)s+(argn+1);}
#endif /* BigStruct */

#define w_get_caraddr(addr,list)    (addr = (PWord *)list)
#define w_get_cdraddr(addr,list)    (addr = (PWord *)list + 1)

int
pbi_mangle()
{				/* mangle(ArgN,Struct,Arg) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    int   arity;
    PWord *argaddr;
    PWord *b;
    PWord *newv3 = NULL;	/* stifle -Wall */

    w_get_An(&v1, &t1, 1);	/* Get argument number  */
    w_get_An(&v2, &t2, 2);	/* Get structure or list */
    w_get_An(&v3, &t3, 3);	/* Get argument to mangle */

    if ((t1 != WTP_INTEGER) || (t3 == WTP_UNBOUND))
	FAIL;

    switch (t2) {
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v2);
	    if (v1 > arity || v1 < 1)
		FAIL;
	    w_get_argaddr(argaddr, v2, (int) v1, arity);
	    break;

	case WTP_LIST:
	    if (v1 == 1)
		w_get_caraddr(argaddr, v2);
	    else if (v1 == 2)
		w_get_cdraddr(argaddr, v2);
	    else
		FAIL;
	    break;

	default:
	    FAIL;
    }

    w_install(argaddr, v3, t3);	/* mangle the new argument */

    /*
     * if the object is an integer, or a symbol, we are done.
     * Otherwise the object is a list, a structure, or an uia.
     * In that case, if the object is above the slot in the heap
     * we have to update "HB" registers and HB values in choice
     * points.
     */

    if ((t3 != WTP_INTEGER) && (t3 != WTP_SYMBOL)) {

	switch (t3) {
	    case WTP_LIST:
	    case WTP_STRUCTURE:
		newv3 = (PWord *) MBIAS(v3);
		break;
	    case WTP_UIA:
		newv3 = (PWord *) MBIAS(M_FIRSTUIAWORD(v3));
		break;
	}

	if (argaddr < newv3) {
	    /*
	     * The object is a list, a structure or an uia, and,
	     * the object is above the slot
	     */
	    gv_setcnt++;
	    wm_HB = wm_H;
	    b = wm_B;
	    while (b != (PWord *) 0 && (PWord) chpt_HB(b) >= (PWord) argaddr) {
		chpt_HB(b) = wm_H;
		chpt_SPB(b) = (PWord *) (((long) chpt_SPB(b)) & ~3);
		b = chpt_B(b);
	    }
	}
    }

    SUCCEED;

}


#if 0
int
pbi_trailed_mangle()
{				/* trailed_mangle(ArgN,Struct,Arg) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    int   arity;
    PWord *argaddr;
    PWord *b;
    PWord *newv3 = NULL;	/* stifle -Wall */

    w_get_An(&v1, &t1, 1);	/* Get argument number  */
    w_get_An(&v2, &t2, 2);	/* Get structure or list */
    w_get_An(&v3, &t3, 3);	/* Get argument to mangle */

    if ((t1 != WTP_INTEGER) || (t3 == WTP_UNBOUND))
	FAIL;

    switch (t2) {
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v2);
	    if (v1 > arity || v1 < 1)
		FAIL;
	    w_get_argaddr(argaddr, v2, (int) v1, arity);
	    break;

	case WTP_LIST:
	    if (v1 == 1)
		w_get_caraddr(argaddr, v2);
	    else if (v1 == 2)
		w_get_cdraddr(argaddr, v2);
	    else
		FAIL;
	    break;

	default:
	    FAIL;
    }
	/* argaddr is the location to be modified;
	   Need to trail this location and move its
           value onto the trail before we modify it
	 */

    w_install(argaddr, v3, t3);	/* mangle the new argument */

    /*
     * if the object is an integer, or a symbol, we are done.
     * Otherwise the object is a list, a structure, or an uia.
     * In that case, if the object is above the slot in the heap
     * we have to update "HB" registers and HB values in choice
     * points.
     */

    if ((t3 != WTP_INTEGER) && (t3 != WTP_SYMBOL)) {

	switch (t3) {
	    case WTP_LIST:
	    case WTP_STRUCTURE:
		newv3 = (PWord *) MBIAS(v3);
		break;
	    case WTP_UIA:
		newv3 = (PWord *) MBIAS(M_FIRSTUIAWORD(v3));
		break;
	}

	if (argaddr < newv3) {
	    /*
	     * The object is a list, a structure or an uia, and,
	     * the object is above the slot
	     */
	    gv_setcnt++;
	    wm_HB = wm_H;
	    b = wm_B;
	    while (b != (PWord *) 0 && (PWord) chpt_HB(b) >= (PWord) argaddr) {
		chpt_HB(b) = wm_H;
		chpt_SPB(b) = (PWord *) (((long) chpt_SPB(b)) & ~3);
		b = chpt_B(b);
	    }
	}
    }

    SUCCEED;

}

#endif





int
pbi_functor()
{				/* functor(Struct,F,A)  */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord f;
    int   a;

    w_get_An(&v1, &t1, 1);	/* get arguments        */
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    /*
     * Switch on the type of the first argument
     */

    switch (t1) {
	case WTP_UIA:
	    if (!force_uia(&v1, &t1)) {
		FAIL;
		break;
	    }
	    /* fall thru to WTP_SYMBOL case */
	case WTP_SYMBOL:
	    if (w_unify(v1, t1, v2, t2) &&
		w_unify((PWord) 0, WTP_INTEGER, v3, t3))
		SUCCEED;
	    else
		FAIL;
	    break;

	case WTP_STRUCTURE:
	    w_get_functor(&f, v1);
	    w_get_arity(&a, v1);

	    if (w_unify(f, WTP_SYMBOL, v2, t2) &&
		w_unify((PWord) a, WTP_INTEGER, v3, t3))
		SUCCEED;
	    else
		FAIL;

	    break;

	case WTP_UNBOUND:
	    if (t3 == WTP_INTEGER && force_uia(&v2, &t2)) {
		PWord s;
		int   t;
		int   i;

/* Creation of lists and structures is fixed. -- Ilyas & Raman 5/16/91 */

		if (v2 == TK_DOT && v3 == 2) {
		    w_mk_list(&s, &t);
		    w_install_unbound_car(s);
		    w_install_unbound_cdr(s);
		}
		else if (v3 == 0) {
		    s = v2;
		    t = WTP_SYMBOL;
		}
		else {
		    w_mk_term(&s, &t, v2, (int) v3);
		    for (i = 1; i <= v3; i++) {
			w_install_unbound_argn(s, i);
		    }
		}

		if (w_unify(v1, t1, s, t))
		    SUCCEED;
		else
		    FAIL;

	    }
	    else
		FAIL;

	    break;

	case WTP_LIST:
	    if (w_unify((PWord) TK_DOT, WTP_SYMBOL, v2, t2) &&
		w_unify((PWord) 2, WTP_INTEGER, v3, t3))
		SUCCEED;
	    else
		FAIL;
	    break;

	default:
	    FAIL;
	    break;

    }
}

int
pbi_identical()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (wm_identical(v1, t1, v2, t2))
	SUCCEED;
    else
	FAIL;
}

int
pbi_unidentical()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (wm_identical(v1, t1, v2, t2))
	FAIL;
    else
	SUCCEED;
}

int
pbi_eq()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == t2 && v1 == v2)
	SUCCEED;
    else
	FAIL;
}

int
pbi_noneq()
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == t2 && v1 == v2)
	FAIL;
    else
	SUCCEED;
}


int
pbi_var()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_UNBOUND)
	SUCCEED;
    else
	FAIL;
}

int
pbi_nonvar()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 != WTP_UNBOUND)
	SUCCEED;
    else
	FAIL;
}

int
pbi_integer()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_INTEGER)
	SUCCEED;
    else
	FAIL;
}

int
pbi_float()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

#ifdef DoubleType
    if (t == WTP_DOUBLE)
	SUCCEED;
#else  /* DoubleType */
    if (t == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, v);
	w_get_functor(&functor, v);

	if (arity == 4 && functor == TK_DDOUBLE)
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else
	FAIL;
}

int
pbi_number()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

#ifdef DoubleType
    if (t == WTP_DOUBLE)
	SUCCEED;
#else  /* DoubleType */
    if (t == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, v);
	w_get_functor(&functor, v);

	if (arity == 4 && functor == TK_DDOUBLE)
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else if (t == WTP_INTEGER)
	SUCCEED;
    else
	FAIL;
}

int
pbi_atom()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_SYMBOL || ta1 == WTP_UIA)
	SUCCEED;
    else
	FAIL;
}

int
pbi_atomic()
{
    PWord va1;
    int   ta1;

    w_get_An(&va1, &ta1, 1);

    if (ta1 == WTP_SYMBOL || ta1 == WTP_INTEGER || ta1 == WTP_UIA)
	SUCCEED;
#ifdef DoubleType
    else if (ta1 == WTP_DOUBLE)
	SUCCEED;
#else  /* DoubleType */
    else if (ta1 == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_arity(&arity, va1);
	w_get_functor(&functor, va1);

	if (arity == 4 && functor == TK_DDOUBLE)
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else
	FAIL;
}

#endif /* CMeta */


/*
 * pbi_findterm seeks a term on the heap
 */

int
pbi_findterm()
{				/* $findterm(F,A,Pos, Term,NewPos) */
    PWord f, a, p, t, n;
    int   ft, at, pt, tt, nt;
    PWord functor;
    PWord *hpos;

    w_get_An(&f, &ft, 1);
    w_get_An(&a, &at, 2);
    w_get_An(&p, &pt, 3);
    w_get_An(&t, &tt, 4);
    w_get_An(&n, &nt, 5);

    if (!xform_uia(&f, &ft) || at != WTP_INTEGER || pt != WTP_INTEGER || p < 0)
	FAIL;

    functor = MMK_FUNCTOR(f, a);
    for (hpos = wm_heapbase + p; hpos < wm_H; hpos++) {
	if (*hpos == functor) {
	    if (w_unify((PWord) hpos, WTP_STRUCTURE, t, tt) &&
	    w_unify((long) (hpos - wm_heapbase) + a + 1, WTP_INTEGER, n, nt))
		SUCCEED;
	    else
		FAIL;
	}
    }

    FAIL;
}
