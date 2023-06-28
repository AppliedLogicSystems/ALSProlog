/*=====================================================================*
 |			butil.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems
 |
 |			-- prolog builtin utilties.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | Revision History: (fixes, not addition of new builtins)
 | 06/28/85	 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85	 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86	 - K. Buettner -- IBM PC conversion
 | 10/26/94	 - C. Houpt -- Various char* and UCHAR* casts.
 *=====================================================================*/
#include "defs.h"

static	void	hc	( PWord *, int *, pword );

/*
 * heap_copy copies structure returned by the parser to the heap
 * creating variables where necessary.
 */

#define MAXVARS 600
static long varptrs[MAXVARS];	/* pointers to words */

static void
hc(rval, rtag, w)
    PWord *rval;		/* value to return */
    int  *rtag;			/* tag to return   */
    pword w;			/* object to copy */
{
    register int i;
    PWord v;
    int   t;

    switch (TYPEOF(w)) {
	case TP_INT:
	    w_mk_int(rval, rtag, INT_VAL(w));
	    return;
	case TP_SYM:
	    w_mk_sym(rval, rtag, FUNCTOR_TOKID(w));
	    return;
	case TP_LIST:
	    w_mk_list(rval, rtag);	/* make a list cell     */
	    hc(&v, &t, LIST_CAR(w));
	    w_install_car(*rval, v, t);
	    hc(&v, &t, LIST_CDR(w));
	    w_install_cdr(*rval, v, t);
	    return;
	case TP_TERM:
	    w_mk_term(rval, rtag, (PWord) FUNCTOR_TOKID(TERM_FUNCTOR(w)),
		      TERM_ARITY(w));

	    for (i = 1; i <= TERM_ARITY(w); i++) {
		hc(&v, &t, TERM_ARGN(w, i));
		w_install_argn(*rval, i, v, t);
	    }
	    return;
	case TP_VO:
	    i = VO_VAL(w);
	    if (varptrs[i] == -1) {
		w_mk_unbound(rval, rtag);
		varptrs[i] = *rval;
	    }
	    else
		*rval = varptrs[i];
	    *rtag = WTP_REF;
	    return;
	case TP_UIA:
	    w_mk_uia(rval, rtag, (UCHAR *)UIA_NAME(w));
	    return;
#ifdef DoubleType
	case TP_DOUBLE:
	    w_mk_double(rval, rtag, DOUBLE_VAL(w));
	    return;
#endif /* DoubleType */
	default:
	    fatal_error(FE_IN_HEAPCOPY, 0);
    }
}

void
heap_copy(rval, rtag, w)
    PWord *rval;
    int  *rtag;
    pword w;
{
    register int i;

    for (i = 0; i < MAXVARS; i++)
	varptrs[i] = -1;

    hc(rval, rtag, w);
}

int
xform_uia(vp, tp)
    PWord *vp;
    int  *tp;
{
    int   tk = 0;

    if (*tp == WTP_UIA) {
	if ( (tk = probe_token((UCHAR *) M_FIRSTUIAWORD(*vp))) ) {
	    *vp = (PWord) tk;
	    *tp = WTP_SYMBOL;
	    return 1;
	}
	else
	    return 0;
    }
    else if (*tp == WTP_SYMBOL)
	return 1;
    else
	return 0;
}

int
force_uia(vp, tp)
    PWord *vp;
    int  *tp;
{
    if (*tp == WTP_UIA) {
	*vp = (PWord) find_token((UCHAR *)M_FIRSTUIAWORD(*vp));
	*tp = WTP_SYMBOL;
	return 1;
    }
    else if (*tp == WTP_SYMBOL)
	return 1;
    else
	return 0;
}

void
string_to_list(l, t, s)
    PWord *l;
    int  *t;
    UCHAR *s;
{
    unsigned char *q;
    PWord lt;
    int   tt;

    for (q = s; *q; q++) ;	/* move q to end of string */

    lt = TK_NIL;
    tt = WTP_SYMBOL;
    while (--q >= s) {
	w_mk_list(l, t);
	w_install_car(*l, (PWord) (*q), WTP_INTEGER);
	w_install_cdr(*l, lt, tt);
	lt = *l;
	tt = *t;
    }
    *l = lt;
    *t = tt;
}

int
list_to_string(s, l, m)
    UCHAR *s;			/* Buffer to copy to */
    PWord l;			/* List pointer */
    int   m;			/* Max length of string */
{
    int   t;
    PWord carv;
    int   cart;

    t = WTP_LIST;		/* Given */
    while (m > 0 && t == WTP_LIST) {
	w_get_car(&carv, &cart, l);

	if (cart != WTP_INTEGER)
	    return (0);

	*s++ = (UCHAR) carv;
	w_get_cdr(&l, &t, l);	/* move on to cdr       */
	m--;
    }

    if (m <= 0 || t != WTP_SYMBOL || l != TK_NIL)
	return (0);
    else {
	*s = '\0';
	return (1);
    }
}

/*
 * getstring(addr,v,t)
 *
 * If the second and third argument are the value and tag of
 * a SYMBOL or UIA, the function sets the contents of addr to
 * to point to the string value of the SYMBOL or UIA. The function
 * returns 1 if successful, and 0 otherwise.
 */

int
getstring(addr, v, t)
    UCHAR **addr;
    PWord v;
    int   t;
{
    if (t == WTP_SYMBOL)
	*addr = TOKNAME(v);
    else if (t == WTP_UIA)
	*addr = (UCHAR *) M_FIRSTUIAWORD(v);
    else
	return 0;

    return 1;
}


/*
 * getlong(ip,v,t)
 *
 *      getlong is given a long pointer (to return the integer value)
 *      and a value and type.  getlong attempts to reconcile the value
 *      and type to an integer.  If successful, this function returns 1,
 *      otherwise 0.
 */

int
getlong(ip, v, t)
    long *ip;
    PWord v;
    int   t;
{
    switch (t) {
	case WTP_INTEGER:
	    *ip = v;
	    return 1;
#ifndef DoubleType
	case WTP_STRUCTURE:{
		double dblval;
		PWord functor, v1;
		int   arity, i, t1;

		w_get_arity(&arity, v);
		w_get_functor(&functor, v);
		if (arity == 4 && functor == TK_DDOUBLE) {
		    for (i = 0; i < 4; i++) {
			w_get_argn(&v1, &t1, v, i + 1);
			*(((short *) &dblval) + i) = (short) v1;
		    }
		    *ip = (long) dblval;
		    return 1;
		}
		else
		    return 0;
	    }
#else
	case WTP_DOUBLE:{
		double dblval;

		w_get_double(&dblval, v);
		*ip = (long) dblval;
		return 1;
	    }
#endif
	default:
	    *ip = 0;
	    return 0;
    }
}


/*
 * getdouble(ip,v,t)
 *
 *      getdouble is given a double pointer (to return the integer value)
 *      and a value and type.  getdouble attempts to reconcile the value
 *      and type to a double.  If successful, this function returns 1,
 *      otherwise 0.
 */

int
getdouble(dp, v, t)
    double *dp;
    PWord v;
    int   t;
{
    switch (t) {
	case WTP_INTEGER:
	    *dp = (double) v;
	    return 1;
#ifndef DoubleType
	case WTP_STRUCTURE:{
		double dblval;
		PWord functor, v1;
		int   arity, i, t1;

		w_get_arity(&arity, v);
		w_get_functor(&functor, v);
		if (arity == 4 && functor == TK_DDOUBLE) {
		    for (i = 0; i < 4; i++) {
			w_get_argn(&v1, &t1, v, i + 1);
			*(((short *) &dblval) + i) = (short) v1;
		    }
		    *dp = dblval;
		    return 1;
		}
		else
		    return 0;
	    }
#else
	case WTP_DOUBLE:{
		double dblval;

		w_get_double(&dblval, v);
		*dp = dblval;
		return 1;
	    }
#endif
	default:
	    *dp = 0;
	    return 0;
    }
}

/*
 * Gets the global variable number of a global variable created with
 * make_gv in the builtins module.
 */

int
get_gv_number(name)
    UCHAR *name;
{
    PWord vArg, vFunctor, vStruct, vName;
    int   tArg, tFunctor, tStruct, tName;
    int   handle, retval;

    PI_makesym(&vFunctor, &tFunctor, "gv_number");
    PI_makestruct(&vStruct, &tStruct, vFunctor, 2);
    PI_makeuia(&vName,&tName,(char *)name);
    w_install_argn(vStruct, 1, vName, tName);
    PI_getargn(&vArg,&tArg,vStruct,2);
    handle = gv_alloc();
    gv_set(vArg,tArg,handle);
    if (PI_rungoal(TK_BUILTINS, vStruct, tStruct)) {
	gv_get(&vArg,&tArg,handle);
	if (tArg != WTP_INTEGER)
	    retval = -1;
	else
	    retval = vArg;
    }
    else
	retval = -1;
    gv_free(handle);
    return retval;
}

void
set_prolog_error(namtok,arity,rfunc,rarity,rsym,v2,t2,v3,t3)
    PWord namtok, rfunc, rsym, v2, v3;
    int arity, rarity, t2, t3;
{
    /*//static long pegvnum = -5;*/	/* something other than -1 */
    if (pegvnum < 0) {
	if (pegvnum == -5)
	    ss_register_global(&pegvnum);
	pegvnum = get_gv_number((UCHAR *)"PrologError");
    }

    if (pegvnum > 0) {
	PWord ev, gv, av, rv;
	int   et, gt, at, rt;

	/*
	 * Build the reason term
	 */
	
	if (rarity == 0) {
	    rv = rfunc;
	    rt = WTP_SYMBOL;
	}
	else {
	    w_mk_term(&rv, &rt, rfunc, rarity);
	    w_install_argn(rv,1,rsym,WTP_SYMBOL);
	    if (rarity >= 2)
		w_install_argn(rv,2,v2,t2);
	    if (rarity >= 3)
		w_install_argn(rv,3,v3,t3);
	}

	/*
	 * Build a term representing goal on which error occurred. This
	 * term will be left in (gv,gt).  
	 */
	
	if (arity > 0) {
	    int i;
	    w_mk_term(&gv,&gt,namtok,arity);
	    for (i=1; i<= arity; i++) {
		w_get_An(&av,&at,i);
		if (at != WTP_UNBOUND)	/* install if not variable */
		    w_install_argn(gv,i,av,at);
		else {		/* unify if var as it might live on stack */
		    PWord vv;
		    int vt;
		    w_install_unbound_argn(gv,i);
		    w_get_argn(&vv,&vt,gv,i);
		    (void) w_unify(av,at,vv,vt);
		}
	    }
	}
	else {
	    gv = namtok;
	    gt = WTP_SYMBOL;
	}

	/*
	 * Build builtins:Goal.  Leave in (av,at).
	 */
	
	w_mk_term(&av,&at,TK_COLON,2);
	w_install_argn(av,1,TK_BUILTINS,WTP_SYMBOL);
	w_install_argn(av,2,gv,gt);

	/*
	 * Put into a list. Leave in (gv,gt).
	 */

	w_mk_list(&gv,&gt);
	w_install_car(gv,av,at);
	w_install_cdr(gv,TK_NIL,WTP_SYMBOL);

	/*
	 * Build the term error/2
	 */
	
	w_mk_term(&ev,&et,TK_ERROR,2);
	w_install_argn(ev,1,rv,rt);
	w_install_argn(ev,2,gv,gt);

	/*
	 * Indicate (to the interrupt system) that an error has occurred.
	 */
	
	gv_set(ev, et, pegvnum);

	wm_safety = -1;
	wm_interrupt_caught = ALSSIG_ERROR;
    }
}

#ifdef macintosh
#pragma export on
#endif

EXPORT ALSPI_API(void)
PI_throw(PWord obj, int objt)
{
	gv_set(obj, objt, get_gv_number((UCHAR *)"PrologError"));

	wm_safety = -1;
	wm_interrupt_caught = ALSSIG_ERROR;
}

EXPORT ALSPI_API(void)
PI_getball(PWord *obj, int *objt)
{
	gv_get(obj, objt, get_gv_number((UCHAR *)"PrologError"));
}

#ifdef macintosh
#pragma export reset
#endif
