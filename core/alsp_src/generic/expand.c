/*=================================================================*
 |			expand.c             
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |			-- dcg and comma expansion
 |
 | Program Author:  Kevin A. Buettner
 | Creation Date: 7/24/85
 | Revision History:
 | 08/10/85 - K. Buettner -- Moved rule and dcg building from parser to expand.c
 | 01/15/86 - K. Buettner -- IBM PC port; removed semicolon expansion
 | 10/26/94 - C. Houpt	  -- Various UCHAR* casts.
 *=================================================================*/
#include "defs.h"

#ifdef XPANDTM

#include "varproc.h"
#include "parsstak.h"
#include "module.h"
#include "compile.h"
#include "icodegen.h"
#include "main.h"

static	void	rxpbod			( pword );
static	pword	expand_pred		( pword );
static	pword	expand_fact		( int, pword );
static	pword	expand_rule		( int, pword );
static	pword	expand_query	( int, pword );
static	pword	expand_command	( int, pword );
static  pword	expand_expand	( int, pword );

/*
 * DCG and Rule Expansion:
 *      The parser returns structured terms (with variable offsets rather
 *      than real variables) and the compiler requires rules in its own
 *      form.  (See alloc.h and the definition of TP_RULE).
 *
 *      The following functions expand DCG's, rules, and facts to a form
 *      palatable by the compiler.
 */

static pword
expand_pred(r)
    pword r;
{
    if (TYPEOF(r) == TP_UIA)
	return (MK_SYM(find_token((UCHAR *)UIA_NAME(r))));
    else
	return r;
}

static pword
expand_fact(nvars, r)
    int   nvars;
    pword r;
{
    pword rv;

    rv = MK_RULE(0);
    RULE_HEAD(rv) = expand_pred(r);
    RULE_NVARS(rv) = nvars;

    return rv;
}

static pword
expand_rule(nvars, r)
    int   nvars;
    pword r;
{
    push_rator(nvars, 0);
    *pst_rand++ = expand_pred(TERM_ARGN(r, 1));
    rxpbod(TERM_ARGN(r, 2));
    bld_clause();
    return (*--pst_rand);
}


static pword
expand_query(nvars, r)
    int   nvars;
    pword r;
{
    push_rator(nvars, 0);
    *pst_rand++ = MK_SYM(TK_QUEST);
    rxpbod(TERM_ARGN(r, 1));
    bld_showanswers();
    bld_clause();
    return (*--pst_rand);
}

static pword
expand_command(nvars, r)
    int   nvars;
    pword r;
{
    push_rator(nvars, 0);
    *pst_rand++ = MK_SYM(TK_RIF);
    rxpbod(TERM_ARGN(r, 1));
    bld_clause();
    return (*--pst_rand);
}

static pword
expand_expand(nvars, r)
    int   nvars;
    pword r;
{
    pword s, t;

    push_rator(nvars, 0);
    *pst_rand++ = MK_SYM(TK_RIF);

    t = MK_TERM(3);
    TERM_FUNCTOR(t) = MK_FUNCTOR(TK_EXPAND, 3);
#ifdef UNIX_DARWIN
    /* Work around a problem in Darwin's preprocessor. */
    TERM_ARGN(t, 1) = MK_FUNCTOR((*top_module), 0);
#else
    TERM_ARGN(t, 1) = MK_SYM(cur_mod);
#endif
    TERM_ARGN(t, 2) = r;
    TERM_ARGN(t, 3) = bld_vlst();

    s = MK_TERM(2);
    TERM_FUNCTOR(s) = MK_FUNCTOR(TK_COLON, 2);
    TERM_ARGN(s, 1) = MK_SYM(MODULE_BUILTINS);
    TERM_ARGN(s, 2) = t;

    *pst_rand++ = s;

    bld_clause();

    return (*--pst_rand);
}

/*
 * A Satan which eats the parsed term and regurgitates material suitable
 * for creating small home appliances.
 *
 * In other words, the term is going to be used as code or a query. So this
 * scans through the parsed term looking for goals, which it then replaces
 * with one where variables as goals are turned into CALLS and lists are
 * turned into consults.
 *
 * -- above description courtesy of Keith Hughes.
 */

void
rxpbod(gs)
    pword gs;
{
    pword t;
    register int tp;
    register int i, n;

    while ((tp = TYPEOF(gs)) == TP_TERM &&
	   FUNCTOR_TOKID(TERM_FUNCTOR(gs)) == TK_COMMA) {
	n = TERM_ARITY(gs);
	for (i = 1; i < n; i++)
	    rxpbod(TERM_ARGN(gs, i));
	gs = TERM_ARGN(gs, n);
    }

    switch (tp) {
	case TP_TERM:
	    *pst_rand++ = gs;
	    break;

	case TP_LIST:
	    while (TYPEOF(gs) == TP_LIST) {
		t = MK_TERM(1);
		TERM_FUNCTOR(t) = MK_FUNCTOR(TK_CONSULT, 1);
		TERM_ARGN(t, 1) = LIST_CAR(gs);
		*pst_rand++ = t;
		gs = LIST_CDR(gs);
	    }
	    break;

	case TP_VO:
	    t = MK_TERM(1);
	    TERM_FUNCTOR(t) = MK_FUNCTOR(TK_CALL, 1);
	    TERM_ARGN(t, 1) = gs;
	    *pst_rand++ = t;
	    break;

	case TP_UIA:
	    *pst_rand++ = MK_SYM(find_token((UCHAR *)UIA_NAME(gs)));
	    break;

	default:
	    *pst_rand++ = gs;
	    break;
    }
}


#define FROM_PARSER 1

void
parser_action(nvars, t)
    int   nvars;
    pword t;
{
    icode(IC_INIT, 0, 0, 0, 0);
    if (TYPEOF(t) == TP_TERM) {
	switch (FUNCTOR_TOKID(TERM_FUNCTOR(t))) {
	    case TK_QUEST:
		if (compile_clause(expand_query(nvars, t), FROM_PARSER))
		    icode(IC_EXECQUERY, 0, 0, 0, 0);
		break;
	    case TK_RIF:
		switch (TERM_ARITY(t)) {
		    case 1:

			/*
			 * execute a command
			 */

			if (compile_clause(expand_command(nvars, t), FROM_PARSER))
			    icode(IC_EXECCOMMAND, 0, 0, 0, 0);
			break;
		    case 2:

			/*
			 * assert a rule
			 */

			if (compile_clause(expand_rule(nvars, t), FROM_PARSER))
			    icode(IC_ADDCLAUSE, 0, 0, 0, 0);
			break;
		    default:

			/*
			 * assert the colon dash thing as a fact
			 */

			if (compile_clause(expand_fact(nvars, t), FROM_PARSER))
			    icode(IC_ADDCLAUSE, 0, 0, 0, 0);
			break;
		}
		break;
	    default:
		if (TOKBINOP(FUNCTOR_TOKID(TERM_FUNCTOR(t))) >> 4 == 1200) {
		    if (compile_clause(expand_expand(nvars, t), FROM_PARSER))
			icode(IC_EXECCOMMAND, 0, 0, 0, 0);
		}
		else {
		    if (compile_clause(expand_fact(nvars, t), FROM_PARSER))
			icode(IC_ADDCLAUSE, 0, 0, 0, 0);
		}
		break;
	}
    }
    else {
	if (compile_clause(expand_fact(nvars, t), FROM_PARSER))
	    icode(IC_ADDCLAUSE, 0, 0, 0, 0);
    }

}



/*
 * Runtime Structure to Compile-time structure converter (for use with assert)
 *
 * cvt_term_to_rule(v,t)   takes a term v with type t and returns a rule made
 *              out of it suitable (though rarely optimal) for compilation.  The
 *              preceding procedures will be called though to flatten out
 *              the rule, thus making it possible for the compiler to work
 *              on it directly.
 *
 */

#ifdef KERNAL
static long vtable[512];
#else
static long vtable[4096];
#endif /* KERNAL */
static int vtp;


static	pword	cvt_walk_term	( PWord );
static	pword	cvt_walk_list	( PWord );
static	pword	cvt_walk_sym	( PWord );
static	pword	cvt_walk_int	( PWord );
static	pword	cvt_walk_var	( PWord );
static	pword	cvt_walk_uia	( PWord );

#ifdef DoubleType
static	pword	cvt_walk_dbl	( PWord );
#endif


static pword (*cvt_walk_tbl[])( PWord ) = {
    	cvt_walk_var,
	cvt_walk_list,
	cvt_walk_term,
	cvt_walk_sym,
	cvt_walk_int,
	cvt_walk_uia
#ifdef DoubleType
	, cvt_walk_dbl
#endif
};

#define CVTWALK(v,t) (*cvt_walk_tbl[(t)])(v)

pword
cvt_term_to_rule(v, t)
    PWord v;
    int   t;
{
    pword r;
    PWord functor;
    int   arity;

    vtp = 0;
    alc_rst();
    parser_reset();
    switch (t) {
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v);
	    w_get_functor(&functor, v);
	    if (functor == TK_RIF && arity == 2) {
		PWord arg;
		int   argt;

		push_rator(0, 0);	/* number of vars not known yet */
		w_get_argn(&arg, &argt, v, 1);

		*pst_rand++ = expand_pred(CVTWALK(arg, argt));

		w_get_argn(&arg, &argt, v, 2);
		rxpbod(CVTWALK(arg, argt));

		bld_clause();
		RULE_NVARS(TOP_RAND) = vtp;
		r = *--pst_rand;

		return (r);
	    }
	    /* fall through */
	case WTP_SYMBOL:
	    r = MK_RULE(0);	/* enough room for head only */
	    RULE_HEAD(r) = CVTWALK(v, t);
	    RULE_NVARS(r) = vtp;
	    return (r);
	    break;
	case WTP_UIA:
	    (void) force_uia(&v, &t);
	    r = MK_RULE(0);	/* enough room for head only */
	    RULE_HEAD(r) = CVTWALK(v, t);
	    RULE_NVARS(r) = vtp;
	    return (r);
	    break;
	default:
	    return (NIL_VAL);
	    break;
    }
}

static pword
cvt_walk_term(v)
    PWord v;
{
    PWord functor, arg;
    int   i, arity, argt;
    pword nt;

    w_get_arity(&arity, v);
    w_get_functor(&functor, v);

    nt = MK_TERM(arity);
    TERM_FUNCTOR(nt) = MK_FUNCTOR((int) functor, arity);

    for (i = 1; i <= arity; i++) {
	w_get_argn(&arg, &argt, v, i);
	TERM_ARGN(nt, i) = CVTWALK(arg, argt);
    }

    return (nt);
}


static pword
cvt_walk_list(v)
    PWord v;
{
    PWord carv, cdrv;
    int   cart, cdrt;

    w_get_car(&carv, &cart, v);
    w_get_cdr(&cdrv, &cdrt, v);

    return (MK_LIST(CVTWALK(carv, cart), CVTWALK(cdrv, cdrt)));
}

static pword
cvt_walk_sym(v)
    PWord v;
{
    return (MK_SYM((int) v));
}

static pword
cvt_walk_int(v)
    PWord v;
{
    return (MK_INT(v));
}


static pword
cvt_walk_uia(v)
    PWord v;
{
    char  buf[512];

    w_get_uianame((UCHAR *)buf, v, 512);

    return (MK_UIA(buf));
}


#ifdef DoubleType
static pword
cvt_walk_dbl(v)
    PWord  v;
{
    double d;

    w_get_double(&d, v);
    return (MK_DOUBLE(d));
}
#endif

static pword
cvt_walk_var(v)
    PWord v;
{
    int   i;

    for (i = 0; i < vtp && vtable[i] != v; i++) ;

    if (i >= vtp) {
	i = vtp++;
	vtable[i] = v;
    }

    return (MK_VO(i));
}
#endif /* XPANDTM */
