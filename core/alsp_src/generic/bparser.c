/*=============================================================================*
 |			bparser.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1993 by Applied Logic Systems
 |
 |			-- Parser Prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | Revision History: (fixes, not addition of new builtins)
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 | 10/26/94 - C. Houpt -- Added char* and UCHAR* casts for various calls.
 *=============================================================================*/
#include "defs.h"

static	int	string_to_number ( UCHAR *, double * );
static	int	get_prec	( PWord, PWord );

/*
 * string_to_number is used by pbi_name to convert a potential string
 *      which represents a number to an actual number.
 *
 *      This function will return 0 if the string did not represent a number,
 *      1 if it did.
 *
 */

static int
string_to_number(register UCHAR *str, double *dblp)
{
    register int c;
    double v;
    int   sign;

    if (*str == '-') {
	sign = -1;
	str++;
    }
    else
	sign = 1;

    /*
     * Get first character.  It must be a digit.
     */

    if ((c = *str) && '0' <= c && c <= '9') {
	v = (double) (c - '0');
	str++;
    }
    else
	return 0;

    /*
     * Consume the next batch of digits
     */

    for (;;) {
	c = *str++;
	if ('0' <= c && c <= '9')
	    v = v * 10 + (double) (c - '0');
	else
	    break;
    }


    /*
     * See if the next character is a dot and process it if it is.
     */

    if (c == '.') {
	double scaleback = 1;

	/*
	 * Consume the next batch of digits.
	 */

	for (;;) {
	    c = *str++;
	    if ('0' <= c && c <= '9') {
		v = v * 10 + (double) (c - '0');
		scaleback *= 10;
	    }
	    else
		break;
	}

	v /= scaleback;		/* adjust v back */
    }

    /*
     * See if the next character is an 'e' (or 'E') and process it if it is.
     */

    if (c == 'e' || c == 'E') {
	int   neg = 0;
	int   exp;
	double m, z;

	if (*str == '-') {
	    neg = 1;
	    str++;
	}
	else if (*str == '+')
	    str++;

	c = *str++;		/* consume first char in exponent */
	if ('0' <= c && c <= '9')
	    exp = (double) (c - '0');
	else
	    return 0;		/* return 0 if not a digit */
	for (;;) {		/* consume rest of chars */
	    c = *str++;
	    if ('0' <= c && c <= '9')
		exp = 10 * exp + (double) (c - '0');
	    else
		break;
	}

	/*
	 * compute the multiplier or divisor
	 */

	m = 1.0;
	z = 10.0;
	while (exp != 0) {
	    if (exp & 1) {
		m *= z;
		exp -= 1;
	    }
	    else {
		z *= z;
		exp >>= 1;
	    }
	}
	if (neg)
	    v /= m;
	else
	    v *= m;
    }

    /*
     * If it is not a null then we have failed to match the entire string
     * with a number.  If it is, then everything is hunky-dory.
     */

    if (c == '\0') {
	*dblp = v * sign;
	return 1;
    }
    else
	return 0;
}


int
pbi_name(void)
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_SYMBOL) {
	PWord slv;
	int   slt;

	string_to_list(&slv, &slt, TOKNAME(v1));
	if (w_unify(v2, t2, slv, slt))
	    SUCCEED;
	else
	    FAIL;
    }
    else if (t1 == WTP_UIA) {
	PWord slv;
	int   slt;

	string_to_list(&slv, &slt, (UCHAR *) M_FIRSTUIAWORD(v1));
	if (w_unify(v2, t2, slv, slt))
	    SUCCEED;
	else
	    FAIL;
    }
    else if (t1 == WTP_INTEGER) {
	PWord slv;
	int   slt;
	char  buf[100];

	sprintf(buf, "%ld", (long) v1);

	string_to_list(&slv, &slt, (UCHAR *)buf);
	if (w_unify(v2, t2, slv, slt))
	    SUCCEED;
	else
	    FAIL;
    }
#ifndef DoubleType
    else if (t1 == WTP_STRUCTURE) {
	PWord functor;
	int   arity;
	double d;
	int   i;
	PWord vt;
	int   tt;
	char  buf[100];

	w_get_arity(&arity, v1);
	w_get_functor(&functor, v1);
	if (arity != 4 || functor != TK_DDOUBLE)
	    FAIL;
	for (i = 0; i < 4; i++) {
	    w_get_argn(&vt, &tt, v1, i + 1);
	    *(((short *) &d) + i) = vt;
	}
	sprintf(buf, "%1.10g", d);
	string_to_list(&vt, &tt, (UCHAR *)buf);
	if (w_unify(v2, t2, vt, tt))
	    SUCCEED;
	else
	    FAIL;
    }
#else  /* DoubleType */
    /* C. Thornley fix for 88k version of floats */
    else if (t1 == WTP_DOUBLE) {
	double d;
	char  buf[100];
	PWord vt;
	int   tt;

	w_get_double(&d, v1);
	sprintf(buf, "%1.10g", d);
	string_to_list(&vt, &tt, (UCHAR *)buf);
	if (w_unify(v2, t2, vt, tt))
	    SUCCEED;
	else
	    FAIL;
    }
#endif /* DoubleType */
    else if (t2 == WTP_LIST) {
	UCHAR *str;

	/* set it up so that the list is converted into a string
	 * on the heap so that later we can convert it to a UIA,
	 * if needed, without having to copy the string.
	 */
	str = (UCHAR *) (wm_H + 1);
	if (list_to_string(str, v2, wm_normal - 256)) {
	    PWord v;
	    int   t;
	    double d;

	    if (string_to_number(str, &d))
			/*!!!!!!!!!!!!!!!!!!!!!!!*/
			/* MAY NEED MODIFICATION */
		make_number(&v, &t, d);
#ifdef AllUIAConsts
	    else {
		v = (PWord) find_token(str);
		t = WTP_SYMBOL;
	    }
#else
	    else if ( (v = probe_token(str)) )
		t = WTP_SYMBOL;
	    else
		w_mk_uia_in_place(&v, &t, str);
#endif
	    if (w_unify(v1, t1, v, t))   
			SUCCEED;
		else
			FAIL;
	}
	else
	    FAIL;

    }
    else if (t2 == WTP_SYMBOL && v2 == TK_NIL &&
	     w_unify(v1, t1, (PWord) find_token((UCHAR *)""), WTP_SYMBOL))
	SUCCEED;
    else
	FAIL;
}


static int
get_prec(PWord assoc, PWord tok)
{
    int   prec, tassoc;

    if (assoc == TK_FX || assoc == TK_FY || assoc == TK_XF || assoc == TK_YF) {
	prec = TOKUNOP(tok);
	if (prec) {
	    tassoc = prec & 7;
	    prec >>= 4;
	    switch (assoc) {
		case TK_FX:
		    if (tassoc != OP_FX(0))
			prec = 0;
		    break;
		case TK_FY:
		    if (tassoc != OP_FY(0))
			prec = 0;
		    break;
		case TK_XF:
		    if (tassoc != OP_XF(0))
			prec = 0;
		    break;
		case TK_YF:
		    if (tassoc != OP_YF(0))
			prec = 0;
		    break;
	    }
	}


    }
    else if (assoc == TK_XFX || assoc == TK_YFX || assoc == TK_XFY) {
	prec = TOKBINOP(tok);
	if (prec) {
	    tassoc = prec & 7;
	    prec >>= 4;
	    switch (assoc) {
		case TK_XFX:
		    if (tassoc != OP_XFX(0))
			prec = 0;
		    break;
		case TK_XFY:
		    if (tassoc != OP_XFY(0))
			prec = 0;
		    break;
		case TK_YFX:
		    if (tassoc != OP_YFX(0))
			prec = 0;
		    break;
	    }
	}
    }
    else
	prec = 0;

    return prec;
}

int
pbi_op(void)
{				/* op(P,A,T) */
    PWord v1, v2, v3;
    int   t1, t2, t3;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UNBOUND && t2 == WTP_SYMBOL && t3 == WTP_SYMBOL) {
	int   prec = get_prec(v2, v3);

	if (prec && w_unify(v1, t1, (PWord) prec, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else if ((t3 == WTP_UIA || t3 == WTP_SYMBOL || t3 == WTP_LIST) &&
	   t2 == WTP_SYMBOL && t1 == WTP_INTEGER && v1 >= 0 && v1 <= 1200) {
	PWord vb;
	int   tb;

	if (t3 == WTP_LIST) {
	    w_get_car(&vb, &tb, v3);
	    w_get_cdr(&v3, &t3, v3);
	}
	else {
	    vb = v3;
	    tb = t3;
	    v3 = TK_NIL;
	    t3 = WTP_SYMBOL;
	}

	while (tb == WTP_SYMBOL || tb == WTP_UIA) {
	    if (tb == WTP_UIA) {
		vb = find_token((UCHAR *) M_FIRSTUIAWORD(vb));
		tb = WTP_SYMBOL;
	    }

	    if (v1 == 0) {
		TOKUNOP(vb) = 0;
		TOKBINOP(vb) = 0;
	    }
	    else {
		switch (v2) {
		    case TK_FX:
			TOKUNOP(vb) = OP_FX(v1);
			break;
		    case TK_FY:
			TOKUNOP(vb) = OP_FY(v1);
			break;
		    case TK_XF:
			TOKUNOP(vb) = OP_XF(v1);
			break;
		    case TK_YF:
			TOKUNOP(vb) = OP_YF(v1);
			break;
		    case TK_XFX:
			TOKBINOP(vb) = OP_XFX(v1);
			break;
		    case TK_XFY:
			TOKBINOP(vb) = OP_XFY(v1);
			break;
		    case TK_YFX:
			TOKBINOP(vb) = OP_YFX(v1);
			break;
		    default:
			break;
		}
	    }

	    if (t3 == WTP_LIST) {
		w_get_car(&vb, &tb, v3);
		w_get_cdr(&v3, &t3, v3);
	    }
	    else
		break;
	}
	SUCCEED;
    }
    else
	FAIL;
}

int
pbi_tokid(void)
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_SYMBOL && w_unify(v1, WTP_INTEGER, v2, t2))
	SUCCEED;
    else
	FAIL;
}


int
pbi_uia_alloc(void)
{				/* $uia_alloc(BufLen,UIABuf) */
    PWord v1, v2;
    int   t1, t2;
    PWord uval;
    int   utag;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_INTEGER && t2 == WTP_UNBOUND && v1 > 0) {
	w_uia_alloc(&uval, &utag, (size_t)v1);
	if (w_unify(v2, t2, uval, utag))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_uia_peekb(void)
{				/* $uia_peekb(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord val;
    UCHAR  pval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER &&
	w_uia_peek(v1, (int) v2, &pval, sizeof (UCHAR))) {
	val = (PWord) pval;
	if (w_unify(v3, t3, val, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_uia_pokeb(void)
{				/* $uia_pokeb(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR  pval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && t3 == WTP_INTEGER) {
	pval = (UCHAR) v3;
	if (w_uia_poke(v1, (int) v2, &pval, sizeof (UCHAR)))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_uia_peekw(void)
{				/* $uia_peekw(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord val;
    short pval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER &&
	w_uia_peek(v1, (int) v2, (UCHAR *) &pval, sizeof (short))) {
	val = (PWord) pval;
	if (w_unify(v3, t3, val, WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_uia_pokew(void)
{				/* $uia_pokew(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    unsigned short pval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && t3 == WTP_INTEGER) {
	pval = (unsigned short) v3;
	if (w_uia_poke(v1, (int) v2, (UCHAR *) &pval, sizeof (short)))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_uia_peekl(void)
{				/* $uia_peekl(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord val;
    int   valtype;
    long  pval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER &&
	w_uia_peek(v1, (int) v2, (UCHAR *) &pval, sizeof (long))) {
		/* Should return integer (long) */
	make_number(&val, &valtype, (double) pval);
	if (w_unify(v3, t3, val, valtype))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_uia_pokel(void)
{				/* $uia_pokel(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    unsigned long pval;
    double dval;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && get_number(v3, t3, &dval)) {
	pval = (unsigned long) dval;
	if (w_uia_poke(v1, (int) v2, (UCHAR *) &pval, sizeof (long)))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}

static int
do_pbi_uia_peekd(PWord v1, int t1, PWord v2, int t2, PWord v3, int t3)
{
    PWord val;
    int   valtype;
    double pval;

    if (t1 == WTP_UIA && t2 == WTP_INTEGER &&
		w_uia_peek(v1, (int) v2, (UCHAR *) &pval, sizeof (double))) 
	{
		make_numberx(&val, &valtype, (double) pval, WTP_DOUBLE);
		if (w_unify(v3, t3, val, valtype))
	    	SUCCEED;
		else
	    	FAIL;
   	}
    else
		FAIL;

}	/* do_pbi_uia_peekd */

int
pbi_uia_peekd(void)
{				/* $uia_peekd(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

	return(do_pbi_uia_peekd(v1,t1, v2,t2, v3,t3 ));

}	/* pbi_uia_peekd */


static int
do_pbi_uia_poked(PWord v1, int t1, PWord v2, int t2, PWord v3, int t3)
{
    double pval;

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && get_number(v3, t3, &pval)) {
		if (w_uia_poke(v1, (int) v2, (UCHAR *) &pval, sizeof (double)))
	    	SUCCEED;
		else
	    	FAIL;
    }
    else
	FAIL;

}	/* do_pbi_uia_poked */

int
pbi_uia_poked(void)
{				/* $uia_poked(UIABuf,Offset,Value) */
    PWord v1, v2, v3;
    int   t1, t2, t3;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

	return( do_pbi_uia_poked(v1,t1, v2,t2, v3,t3) );

}	/* pbi_uia_poked */


int
pbi_uia_peek(void)
{				/* $uia_peek(UIABuf,Offset,Size,Val) */
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    PWord val;
    int   valtype;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && t3 == WTP_INTEGER && v3 > 0) {
	w_uia_alloc(&val, &valtype, (size_t)v3);
	if (w_uia_peek(v1, (int) v2, (UCHAR *) M_FIRSTUIAWORD(val), (int) v3))
	    if (w_unify(v4, t4, val, valtype))
		SUCCEED;
    }

    FAIL;
}


int
pbi_uia_poke(void)
{			/* $uia_poke(UIABuf,Offset,Size,Val,ValOffset) */
    PWord v1, v2, v3, v4, v5;
    int   t1, t2, t3, t4, t5;
    UCHAR *val;
    int val_size;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if (   t1 == WTP_UIA 
	&& t2 == WTP_INTEGER 
	&& t3 == WTP_INTEGER && v3 > 0
	&& getstring(&val,v4,t4) 
	&& t5 == WTP_INTEGER && v5 >= 0) {

	if (t4 == MTP_UIA)
	    val_size = M_UIASIZE(v4);
	else
	    val_size = strlen((char *) val);

	if (   v5 <= val_size
	    && v3 <= val_size - v5
	    && w_uia_poke(v1, (int) v2, (UCHAR *) val+v5, (int) (v3)) )
	    SUCCEED;
    }

    FAIL;
}


int
pbi_uia_peeks(void)
{				/* $uia_peeks(UIABuf,Offset,Sym) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord val;
    int   valtype;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER) {
	str = (UCHAR *) (wm_H + 1);
	if (w_uia_peeks(v1, (int) v2, str, wm_normal - 256)) {
	    if ( (val = probe_token(str)) )
		valtype = WTP_SYMBOL;
	    else
		w_mk_uia_in_place(&val, &valtype, str);

	    if (w_unify(v3, t3, val, valtype))
		SUCCEED;
	}
    }
    FAIL;
}


int
pbi_uia_peeks4(void)
{				/* $uia_peeks(UIABuf,Offset,Size,Sym) */
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    PWord val;
    int   valtype;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && t3 == WTP_INTEGER && v3 > 0) {
	str = (UCHAR *) (wm_H + 1);
	if (w_uia_peeks(v1, (int) v2, str, (int) v3)) {
	    if ( (val = probe_token(str)) )
		valtype = WTP_SYMBOL;
	    else
		w_mk_uia_in_place(&val, &valtype, str);

	    if (w_unify(v4, t4, val, valtype))
		SUCCEED;
	}
    }
    FAIL;
}


int
pbi_uia_pokes(void)
{				/* $uia_pokes(UIABuf,Offset,Sym) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && getstring(&str, v3, t3)) {
	if (w_uia_pokes(v1, (int) v2, str))
	    SUCCEED;
    }

    FAIL;
}


int
pbi_uia_clip(void)
{				/* $uia_clip(UIABuf,Size) */
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UIA && t2 == WTP_INTEGER && v2 > 0 && w_uia_clip(v1, v2))
	SUCCEED;
    else
	FAIL;
}


int
pbi_uia_size(void)
{				/* $uia_size(UIABuf,Size) */
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UIA && w_unify(v2, t2, (PWord) M_UIASIZE(v1), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

int
get_number(PWord v, int t, double *val)
{
    if (t == WTP_INTEGER)
	*val = (double) v;
#ifndef DoubleType
    else if (t == WTP_STRUCTURE) {
	PWord functor;
	int   arity;

	w_get_functor(&functor, v);
	w_get_arity(&arity, v);
	if (arity == 4 && functor == TK_DDOUBLE) {
	    PWord v1;
	    int   t1;
	    int   i;

	    for (i = 0; i < 4; i++) {
		w_get_argn(&v1, &t1, v, i + 1);
		*(((short *) val) + i) = v1;
	    }
	}
    }
#else  /* DoubleType */
    else if (t == WTP_DOUBLE)
	w_get_double(val, v);
#endif /* DoubleType */
    else
	return (0);

    return (1);
}

int
pbi_atom_concat(void)
{				/* $atom_concat(In1,In2,Out) */
    PWord v1, v2, v3, vConcat;
    int   t1, t2, t3, tConcat;
    UCHAR *str1, *str2;
    register UCHAR *p1, *p2, *pConcat;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (!getstring(&str1, v1, t1) || !getstring(&str2, v2, t2))
	FAIL;

    p1 = str1;
    p2 = str2;
    w_uia_alloc(&vConcat, &tConcat, (size_t)(strlen((char *)p1) + strlen((char *)p2)));
    pConcat = (UCHAR *) M_FIRSTUIAWORD(vConcat);
    while ( (*pConcat++ = *p1++) ) ;
    pConcat--;
    while ( (*pConcat++ = *p2++) ) ;

    if (w_unify(v3, t3, vConcat, tConcat))
	SUCCEED;
    else
	FAIL;
}

/*
 * $sub_atom(Atom,Start,Length,SubAtom)
 * $sub_atom(+atom,+integer,+integer,?atom)
 */

int
pbi_sub_atom(void)
{
    PWord v1, v2, v3, v4, vSA;
    int   t1, t2, t3, t4, tSA;
    int len;
    UCHAR *str;

    w_get_An(&v1,&t1,1);
    w_get_An(&v2,&t2,2);
    w_get_An(&v3,&t3,3);
    w_get_An(&v4,&t4,4);

    if (!getstring(&str, v1, t1))
	FAIL;
    
    len = strlen((char *)str);

    if (   t2 != WTP_INTEGER  || t3 != WTP_INTEGER
	|| v2 < 1 || v2 > len+1 || v3 < 0 || v3 > len-v2+1)
	FAIL;
    
    w_uia_alloc(&vSA, &tSA, (size_t)v3);
    strncpy((char *) M_FIRSTUIAWORD(vSA), (char *)str+v2-1, (size_t)v3);

    /* Need to null terminate since strncpy does not guarantee null
     * termination.
     */
    ((char *) M_FIRSTUIAWORD(vSA))[v3] = 0;

    if (w_unify(v4,t4,vSA,tSA))
	SUCCEED;
    else
	FAIL;
}


/*
 * atom_length/2
 *
 * atom_length(Atom,Length) is true iff Length equals the number of
 * characters in Atom.
 *
 * atom_length(+atom, ?integer)
 *
 * Note: This is our first trial of our new error handling mechanism for
 * C-defined builtins.
 */

int
pbi_atom_length(void)
{
    PWord v1, v2;
    int   t1, t2;
    UCHAR *str;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UNBOUND)
	PERR_INSTANTIATION(TK_ATOM_LENGTH,2);

    if (!getstring(&str, v1, t1))
	PERR_TYPE(TK_ATOM_LENGTH,2,TK_ATOM,v1,t1);
    
    if (t2 != WTP_UNBOUND && t2 != WTP_INTEGER)
	PERR_TYPE(TK_ATOM_LENGTH,2,TK_INTEGER,v2,t2);

    if (w_unify(v2, t2, (PWord) strlen((char *)str), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}


/*
 * char_code/2
 *
 * char_code(Char, Code) is true iff the character code for the character
 * Char is Code.
 *
 * Procedurally, char_code(Char, Code) unifies Code with character code
 * for the character Char.
 *
 * char_code(+character, +character_code)
 * char_code(+character, -character_code)
 * char_code(-character, +character_code)
 */

int
pbi_char_code(void)
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UNBOUND && t2 == WTP_UNBOUND)
	PERR_INSTANTIATION(TK_CHAR_CODE,2);
    
    if (t1 != WTP_UNBOUND && (t1 != WTP_SYMBOL || !is_char_tok(v1))
	&& (t1 != WTP_UIA || ((char *)M_FIRSTUIAWORD(v1))[1] != 0))
	PERR_TYPE(TK_CHAR_CODE,2,TK_CHARACTER,v1,t1);
    
    if (t2 != WTP_UNBOUND && (t2 != WTP_INTEGER))
	PERR_TYPE(TK_CHAR_CODE,2,TK_INTEGER,v2,t2);
    
    if (t2 == WTP_INTEGER && (v2 < 0 || v2 >= N_TOK_CHARS))
	PERR_REPRESENTATION(TK_CHAR_CODE,2, TK_CHARACTER_CODE);

    if (t1 == WTP_SYMBOL) {
	if (w_unify(v2,t2,tok_to_char(v1),WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else if (t1 == WTP_UIA) {
	if (w_unify(v2,t2,((unsigned char *)M_FIRSTUIAWORD(v1))[0],WTP_INTEGER))
	    SUCCEED;
	else
	    FAIL;
    }
    else {
	if (w_unify(v1,t1,char_to_tok(v2),WTP_SYMBOL))
	    SUCCEED;
	else
	    FAIL;
    }
}

enum C_list_codes {
    C_list_char,
    C_list_code
};

static int string_to_C_list ( PWord *, int *, UCHAR *, enum C_list_codes );

static int
string_to_C_list(PWord *vp,int *tp,UCHAR *str,enum C_list_codes C_which)
{
    register PWord *H, *lim;
    if (!*str) {
	*vp = TK_NIL;
	*tp = WTP_SYMBOL;
    }
    else {
	*vp = (PWord) (H = wm_H);
	*tp = WTP_LIST;
	lim = (PWord *) ((UCHAR *) wm_TR - 128);
	for (;;) {
	    if (C_which == C_list_char)
		*H++ = MMK_SYM(char_to_tok(*str));
	    else
		*H++ = MMK_INT(*str);
	    str++;
	    if (!*str)
		break;
	    *H = MMK_LIST((PWord) (H+1));
	    H++;
	    if (H > lim)
		return 0;
	}
	*H++ = MMK_SYM(TK_NIL);
	wm_H = H;
    }
    return 1;
}

enum ltos_retcodes {
    ltos_RESOURCE,
    ltos_DOMAIN,
    ltos_TYPE,
    ltos_SUCCESS
};

static enum ltos_retcodes C_list_to_string ( PWord *, int *, PWord, int, enum C_list_codes );

static enum ltos_retcodes
C_list_to_string(PWord *vp,int *tp,PWord vin,int tin,enum C_list_codes C_which)
{
    PWord va;
    int ta;
    register UCHAR *s, *lim;
    UCHAR *str;

    str = s = (UCHAR *) (wm_H + 1);

    lim = (UCHAR *) (wm_TR) - 128;	/* leave a small gap */

    while (tin == WTP_LIST) {
	if (s > lim)
	    return ltos_RESOURCE;
	w_get_car(&va,&ta,vin);
	if (C_which == C_list_char) {
	    if (ta == WTP_SYMBOL && is_char_tok(va))
		*s++ = tok_to_char(va);
	    else if (ta == WTP_UIA && ((char *) M_FIRSTUIAWORD(va))[1] == 0)
		*s++ = *((UCHAR *) M_FIRSTUIAWORD(va));
	    else {
		*vp = vin;
		*tp = tin;
		return ltos_DOMAIN;
	    }
		
	}
	else {
	    if (ta != WTP_INTEGER || !is_char_code(va)) {
		*vp = vin;
		*tp = tin;
		return ltos_DOMAIN;
	    }
	    *s++ = va;
	}
	w_get_cdr(&vin,&tin,vin);
    }

    if (tin != WTP_SYMBOL || vin != TK_NIL) {
	*vp = vin;
	*tp = tin;
	return ltos_TYPE;
    }
    *s = 0;
    w_mk_uia_in_place(vp, tp, str);
    return ltos_SUCCESS;
}

/*
 * atom_chars/2
 *
 * atom_chars(Atom, List) is true iff List is a list whose elements are
 * the characters corresponding to the successive characters of atom Atom.
 *
 * atom_chars(+atom,+list)
 * atom_chars(+atom,-list)
 * atom_chars(-atom,+list)
 */

int
pbi_atom_chars(void)
{
    PWord v1, v2, vr;
    int t1, t2, tr;
    UCHAR *str = 0;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UNBOUND && t2 == WTP_UNBOUND)
	PERR_INSTANTIATION(TK_ATOM_CHARS,2);
    
    if (t1 == WTP_UIA)
        str = (UCHAR *) M_FIRSTUIAWORD(v1);
    else if (t1 == WTP_SYMBOL)
	str = (UCHAR *) TOKNAME(v1);
    else if (t1 != WTP_UNBOUND)
	PERR_TYPE(TK_ATOM_CHARS,2,TK_ATOM,v1,t1);
    
    if (t2 != WTP_LIST && (t2 != WTP_SYMBOL || v2 != TK_NIL)
	               && t2 != WTP_UNBOUND)
	PERR_TYPE(TK_ATOM_CHARS,2,TK_LIST,v2,t2);
    
    if (str) {
	if (!string_to_C_list(&vr,&tr,str,C_list_char))
	    PERR_RESOURCE(TK_ATOM_CHARS,2,TK_HEAPUSED);
	
	if (w_unify(vr,tr,v2,t2))
	    SUCCEED;
	else
	    FAIL;
    }
    else {
	switch (C_list_to_string(&vr,&tr,v2,t2,C_list_char)) {
	    case ltos_RESOURCE :
		PERR_RESOURCE(TK_ATOM_CHARS,2,TK_HEAPUSED);
		break;
	    case ltos_TYPE :
		PERR_TYPE(TK_ATOM_CHARS,2,TK_LIST,vr,tr);
		break;
	    case ltos_DOMAIN :
		PERR_DOMAIN(TK_ATOM_CHARS,2,TK_CHARACTER_LIST,vr,tr);
		break;
	    case ltos_SUCCESS :
	    default :
		if (w_unify(vr,tr,v1,t1))
		    SUCCEED;
		else
		    FAIL;
	}
    }
}

/*
 * atom_codes/2
 *
 * atom_codes(Atom,List) is true iff List is a list whose elements 
 * correspond to the successive characters of atom Atom, and the value of
 * each element is the character code for the corresponding character.
 *
 * atom_codes(+atom, +list)
 * atom_codes(+atom, -list)
 * atom_codes(-atom, +list)
 */

int
pbi_atom_codes(void)
{
    PWord v1, v2, vr;
    int t1, t2, tr;
    UCHAR *str = 0;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 == WTP_UNBOUND && t2 == WTP_UNBOUND)
	PERR_INSTANTIATION(TK_ATOM_CODES,2);
    
    if (t1 == WTP_UIA)
        str = (UCHAR *) M_FIRSTUIAWORD(v1);
    else if (t1 == WTP_SYMBOL)
	str = (UCHAR *) TOKNAME(v1);
    else if (t1 != WTP_UNBOUND)
	PERR_TYPE(TK_ATOM_CODES,2,TK_ATOM,v1,t1);
    
    if (t2 != WTP_LIST && (t2 != WTP_SYMBOL || v2 != TK_NIL)
	               && t2 != WTP_UNBOUND)
	PERR_TYPE(TK_ATOM_CODES,2,TK_LIST,v2,t2);
    
    if (str) {
	if (!string_to_C_list(&vr,&tr,str,C_list_code))
	    PERR_RESOURCE(TK_ATOM_CODES,2,TK_HEAPUSED);
	
	if (w_unify(vr,tr,v2,t2))
	    SUCCEED;
	else
	    FAIL;
    }
    else {
	switch (C_list_to_string(&vr,&tr,v2,t2,C_list_code)) {
	    case ltos_RESOURCE :
		PERR_RESOURCE(TK_ATOM_CODES,2,TK_HEAPUSED);
		break;
	    case ltos_TYPE :
		PERR_TYPE(TK_ATOM_CODES,2,TK_LIST,vr,tr);
		break;
	    case ltos_DOMAIN :
		PERR_DOMAIN(TK_ATOM_CODES,2,TK_CHARACTER_CODE_LIST,vr,tr);
		break;
	    case ltos_SUCCESS :
	    default :
		if (w_unify(vr,tr,v1,t1))
		    SUCCEED;
		else
		    FAIL;
	}
    }
}


/*-------------------------------------------------------------------*
 *-------------------------------------------------------------------*/


#ifdef SUBTYPES

int
pbi_less_sut_int(void)
{				/* less_sut_int(A,B) */
    PWord v1, v2;
    int   t1, t2;

    long val1,val2;
    int   val1stype, val2stype;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_UIA || t2 != WTP_UIA) 
	    FAIL;

	val1stype =  (int) M_FIRSTUIAWORD(v1);
	val2stype =  (int) M_FIRSTUIAWORD(v2);
    if (val1stype != SUT_INT || val1stype != SUT_INT) 
	    FAIL;

	val1 = ((long)((char *)wm_heapbase + (long)(v1))+2);
	val2 = ((long)((char *)wm_heapbase + (long)(v2))+2);

	if ( val1 < val2 )
	    SUCCEED;
	else
	    FAIL;
}

int
pbi_eq_sut_int(void)
{				/* eq_sut_int(A,B) */
    PWord v1, v2;
    int   t1, t2;

    long val1,val2;
    int   val1stype, val2stype;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_UIA || t2 != WTP_UIA) 
	    FAIL;

	val1stype =  (int) M_FIRSTUIAWORD(v1);
	val2stype =  (int) M_FIRSTUIAWORD(v2);
    if (val1stype != SUT_INT || val1stype != SUT_INT) 
	    FAIL;

	val1 = ((long)((char *)wm_heapbase + (long)(v1))+2);
	val2 = ((long)((char *)wm_heapbase + (long)(v2))+2);

	if ( val1 == val2 )
	    SUCCEED;
	else
	    FAIL;
}

extern void    w_mk_sut_int ( PWord *, int *, long );

void
w_mk_sut_int(PWord *rval, int  *rtag, long ival)
{
    *rval = (PWord) MMK_UIAVAL(wm_H);
	*rtag = WTP_UIA;

	*wm_H++ = MMK_FENCE(2);
	*wm_H++ = SUT_INT;
	*wm_H++ = ival;
	*wm_H++ = MMK_FENCE(2);
}


	/* Make a uia_int whose (long) int value is the
	   same as the value in the incoming prolog integer 
	   in arg #1 */
int
pbi_mk_sut_int(void)
{				/* mk_sut_int(A,B) */
    PWord v1, v2, new;
    int   t1, t2, newt;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER || (t2 != WTP_UIA && t2 != WTP_UNBOUND) ) 
	    FAIL;

	w_mk_sut_int(&new, &newt, (long)v1);
	if (w_unify(v2,t2,new,newt))
	    SUCCEED;
	else
	    FAIL;
}

int
pbi_t_sut_int(void)
{
	long i,m;

	m=1;
	i=0;
	printf("C[%d]=%d\n",(int)i,(int)m);
	for (i=1; i < 32; i++) {
		m=m*2;
		printf("C[%d]=%d\n",(int)i,(int)m);
	}
	SUCCEED;
}

int
pbi_pos_atom(void)
{
    PWord v1, v2, new;
    int   t1, t2, newt;
    long val2, baseuia; 
    int   val1stype;
	UCHAR *buf;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_UIA || (t2 != WTP_UIA && t2 != WTP_UNBOUND)) 
	    FAIL;
	baseuia = (long)wm_heapbase + (long)(v1);

/*
printf("baseuia=%lx  *baseuia=%d \n",baseuia, (int)((PWord)(*(PWord *)baseuia)));
printf("baseuia1=%lx  *baseuia1=%d \n", 
			(long)baseuia + sizeof (long), 
			(int)(*(PWord *)(baseuia + sizeof (long))));
printf("baseuia2=%lx  *baseuia2=%d \n", 
			(long)baseuia + 2 * (sizeof (long)),
			(int)(*(PWord *)( baseuia + 2 * (sizeof (long)))));
*/

	val1stype = (int) *(PWord *)(baseuia + sizeof (long));
    if (val1stype != SUT_INT) 
	    FAIL;

	val2 = *(PWord *)( baseuia + 2 * (sizeof (long)));
	buf = (UCHAR *) (wm_H + 1);
	sprintf((char *)buf, "%ld", val2);

	w_mk_uia_in_place(&new, &newt, buf);
	if (w_unify(v2, t2, new, newt))
		SUCCEED;
	else
		FAIL;
}
#endif /* SUBTYPES */
