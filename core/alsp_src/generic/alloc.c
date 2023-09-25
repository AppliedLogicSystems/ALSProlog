/*===================================================================*
 |			alloc.c      
 |      Copyright (c) 1985 by Kevin A. Buettner
 |      Copyright (c) 1986-95 by Applied Logic Systems, Inc.
 |
 |	-- storage allocator and functions which operate on compile
 |         time structures
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/15/85
 | 01/13/86 - K.A. Buettner  -- IBM PC port for ALS
 | 12/06/89 - K.A. Buettner -- Merge of 386, 68k, and 88k
 *===================================================================*/

#include "defs.h"
#include <math.h>		/* for floor */

pwrd  nil_val =
{TP_NIL, 0};

pword prs_area;
static pword areap;

static	pword	allocat	( size_t );

void
prs_area_init(unsigned long marksize)
{
    if (marksize < sizeof (pwrd) * PARSER_AREASIZ)
	marksize = sizeof (pwrd) * PARSER_AREASIZ;

#ifdef AtariOS
    marksize += sizeof (long);
#endif

    if ((prs_area = (pword) malloc((size_t)marksize)) == (pword) 0) {
	fprintf(stderr, "Error: Unable to allocate the parser area.\n");
	als_exit(1);
    }

#ifdef AtariOS
    prs_area = (pword) (((long) prs_area + 3) & ~3L);
#endif
    areap = prs_area;
}

static pword
allocat(size_t size)
{
    register pword t;

    t = areap;
    areap += size;
    if (areap > prs_area + PARSER_AREASIZ) {
	fprintf(stderr, "Error: Parser area exhausted.\n");
	als_exit(1);
    }

    return (t);
}

void
alc_rst(void)
{
    areap = prs_area;
}

pword
mk_term(long arity)
{
    register pword t;

    t = allocat((size_t) arity + 1);
    t->tag = TP_TERM;

    return t;
}

pword
mk_functor(long tkid, long arity)
{
    register pword f;

    f = allocat(2);
    f->tag = TP_SYM;
    f->val = (pword) tkid;
    (f + 1)->val = (pword) arity;

    return f;
}

pword
mk_list(pword car, pword cdr)
{
    register pword c;

    c = allocat(2);
    c->tag = TP_LIST;
    c->val = car;
    (c + 1)->val = cdr;

    return c;
}

pword
mk_int(long iv)
{
    register pword h;

    h = allocat(1);
    h->tag = TP_INT;
    h->val = (pword) iv;

    return h;
}


pword
mk_vo(long vo)
{
    register pword z;

    z = allocat(1);
    z->tag = TP_VO;
    z->val = (pword) vo;

    return z;
}

pword
mk_rule(long ng)
{
    register pword rv;

    rv = allocat((size_t)ng + 2);
    rv->tag = TP_RULE;
    rv->val = (pword) ng;

    return rv;
}

pword
mk_double(double dbl)
{
    register pword d;

#ifdef COERCE2INTS
    if (dbl == floor(dbl) && MINPROLOGINT <= dbl && dbl <= MAXPROLOGINT)
	d = MK_INT(floor(dbl));
    else 
#endif
	{
#ifdef DoubleType
	d = allocat(2);
	d->tag = TP_DOUBLE;
	d->val = (pword) * (long *) &dbl;
	(d + 1)->val = (pword) * (((long *) &dbl) + 1);
#else
	register int i;
	d = MK_TERM(4);
	TERM_FUNCTOR(d) = MK_FUNCTOR(TK_DDOUBLE, 4);
	for (i = 0; i < 4; i++)
	    TERM_ARGN(d, i + 1) = MK_INT(((short *) &dbl)[i]);
#endif
    }

    return d;
}

#ifdef DoubleType
double
double_val(pword p)
{
    double d;

    *(long *) &d = DOUBLE_VAL1(p);
    *(((long *) &d) + 1) = DOUBLE_VAL2(p);
    return d;
}
#else
double
double_val(pword p)
{
	double dblval;
	int i;
	
	for (i = 0; i < 4; i++) {
	  	*(((short *) &dblval) + i) = INT_VAL(TERM_ARGN(p, i + 1));
	}
	return dblval;
}
#endif


/*
 * functor_id_of_term takes a term (not necessarily structured) and returns
 * the token index of functor.
 */

long
functor_id_of_term(pword p)
{
    switch (TYPEOF(p)) {
	case TP_TERM:
	    return FUNCTOR_TOKID(TERM_FUNCTOR(p));
	case TP_LIST:
	    return TK_DOT;
	case TP_SYM:
	    return FUNCTOR_TOKID(p);
	default:
	    return TK_NIL;
    }
}

long
arity_of_term(pword p)
{
    switch (TYPEOF(p)) {
	case TP_TERM:
	    return TERM_ARITY(p);
	case TP_LIST:
	    return 2;
	default:
	    return 0;
    }
}


/*
 * is_double returns 1 if its second argument is a double, 0 otherwise.
 *      It will return the value of the double through the double pointer
 *      when the second argument is a double.
 */

int
is_double(double *d, pword t)
{
#ifdef DoubleType
    if (TYPEOF(t) == TP_DOUBLE) {
	*d = DOUBLE_VAL(t);
	return 1;
    }
#else
    if (TYPEOF(t) == TP_TERM && TERM_ARITY(t) == 4 &&
	FUNCTOR_TOKID(TERM_FUNCTOR(t)) == TK_DDOUBLE) {
	int   i;
	for (i = 0; i < 4; i++)
	    ((short *) d)[i] = INT_VAL(TERM_ARGN(t, i + 1));

	return 1;
    }
#endif
    else
	return 0;
}


pword
mk_uia(char *s)
{
    pword f;
    register char *t;
    long l;

    l = strlen(s) + 1;
    f = allocat((l / sizeof (pwrd)) + (l % sizeof (pwrd) != 0) + 1);

    f->tag = TP_UIA;
    f->val = (pword) l;
    t = (char *) (f + 1);
    while ( (*t++ = *s++) ) ;

    return f;
}
