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

//#include "varproc.h"
//#include "parsstak.h"
#include "module.h"
#include "compile.h"
#include "icodegen.h"
#include "main.h"

static	void	rxpbod			(PE, pword );
static	pword	expand_pred		(PE, pword );
static	pword	expand_fact		(PE,  int, pword );
static	pword	expand_rule		(PE, int, pword );
static	pword	expand_query	(PE, int, pword );
static	pword	expand_command	(PE, int, pword );
static  pword	expand_expand	(PE, int, pword );

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
expand_pred(
    PE,
    pword r
)
{
    if (TYPEOF(r) == TP_UIA)
	return (MK_SYM(find_token((UCHAR *)UIA_NAME(r))));
    else
	return r;
}

static pword
expand_fact(PE, int   lnvars, pword r)
{
    pword rv;

    rv = MK_RULE(0);
    RULE_HEAD(rv) = expand_pred(hpe, r);
    RULE_NVARS(rv) = lnvars;

    return rv;
}

static pword
expand_rule(PE, int lnvars, pword r)
{
    push_rator(hpe, lnvars, 0);
    *pst_rand++ = expand_pred(hpe, TERM_ARGN(r, 1));
    rxpbod(hpe, TERM_ARGN(r, 2));
    bld_clause(hpe);
    return (*--pst_rand);
}


static pword
expand_query(
    PE,
    int  lnvars,
    pword r
)
{
    push_rator(hpe, lnvars, 0);
    *pst_rand++ = MK_SYM(TK_QUEST);
    rxpbod(hpe, TERM_ARGN(r, 1));
    bld_showanswers(hpe);
    bld_clause(hpe);
    return (*--pst_rand);
}

static pword
expand_command(PE, int lnvars, pword r)
{
    push_rator(hpe, lnvars, 0);
    *pst_rand++ = MK_SYM(TK_RIF);
    rxpbod(hpe, TERM_ARGN(r, 1));
    bld_clause(hpe);
    return (*--pst_rand);
}

static pword
expand_expand(
    PE,
    int   lnvars,
    pword r
)
{
    pword s, t;

    push_rator(hpe, lnvars, 0);
    *pst_rand++ = MK_SYM(TK_RIF);

    t = MK_TERM(3);
    TERM_FUNCTOR(t) = MK_FUNCTOR(TK_EXPAND, 3);
    TERM_ARGN(t, 1) = MK_SYM(cur_mod);
    TERM_ARGN(t, 2) = r;
    TERM_ARGN(t, 3) = bld_vlst(hpe);

    s = MK_TERM(2);
    TERM_FUNCTOR(s) = MK_FUNCTOR(TK_COLON, 2);
    TERM_ARGN(s, 1) = MK_SYM(MODULE_BUILTINS);
    TERM_ARGN(s, 2) = t;

    *pst_rand++ = s;

    bld_clause(hpe);

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
rxpbod(PE, pword gs)
{
    pword t;
    register int tp;
    register int i, n;

    while ((tp = TYPEOF(gs)) == TP_TERM &&
	   FUNCTOR_TOKID(TERM_FUNCTOR(gs)) == TK_COMMA) {
	n = TERM_ARITY(gs);
	for (i = 1; i < n; i++)
	    rxpbod(hpe, TERM_ARGN(gs, i));
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
parser_action(PE, int lnvars, pword t)
{
    icode(IC_INIT, 0, 0, 0, 0);
    if (TYPEOF(t) == TP_TERM) {
	switch (FUNCTOR_TOKID(TERM_FUNCTOR(t))) {
	    case TK_QUEST:
		if (compile_clause(hpe, expand_query(hpe, lnvars, t), FROM_PARSER))
		    icode(IC_EXECQUERY, 0, 0, 0, 0);
		break;
	    case TK_RIF:
		switch (TERM_ARITY(t)) {
		    case 1:

			/*
			 * execute a command
			 */

			if (compile_clause(hpe, expand_command(hpe, lnvars, t), FROM_PARSER))
			    icode(IC_EXECCOMMAND, 0, 0, 0, 0);
			break;
		    case 2:

			/*
			 * assert a rule
			 */

			if (compile_clause(hpe, expand_rule(hpe, lnvars, t), FROM_PARSER))
			    icode(IC_ADDCLAUSE, 0, 0, 0, 0);
			break;
		    default:

			/*
			 * assert the colon dash thing as a fact
			 */

			if (compile_clause(hpe, expand_fact(hpe, lnvars, t), FROM_PARSER))
			    icode(IC_ADDCLAUSE, 0, 0, 0, 0);
			break;
		}
		break;
	    default:
		if (TOKBINOP(FUNCTOR_TOKID(TERM_FUNCTOR(t))) >> 4 == 1200) {
		    if (compile_clause(hpe, expand_expand(hpe, lnvars, t), FROM_PARSER))
			icode(IC_EXECCOMMAND, 0, 0, 0, 0);
		}
		else {
		    if (compile_clause(hpe, expand_fact(hpe, lnvars, t), FROM_PARSER))
			icode(IC_ADDCLAUSE, 0, 0, 0, 0);
		}
		break;
	}
    }
    else {
	if (compile_clause(hpe, expand_fact(hpe, lnvars, t), FROM_PARSER))
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
//static long evtable[512];
#else
//static long evtable[4096];
#endif /* KERNAL */
//static int vtp;


static	pword	cvt_walk_term	(PE, PWord );
static	pword	cvt_walk_list	(PE,  PWord );
static	pword	cvt_walk_sym	(PE,  PWord );
static	pword	cvt_walk_int	(PE,  PWord );
static	pword	cvt_walk_var	(PE,  PWord );
static	pword	cvt_walk_uia	(PE,  PWord );

#ifdef DoubleType
static	pword	cvt_walk_dbl	(PE,  PWord );
#endif


static pword (*cvt_walk_tbl[])(PE, PWord ) = {
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

#define CVTWALK(v,t) (*cvt_walk_tbl[(t)])(hpe, v)

pword
cvt_term_to_rule(PE, PWord v, int t)
{
    pword r;
    PWord functor;
    int   arity;

    vtp = 0;
    alc_rst(hpe);
    parser_reset(hpe);
    switch (t) {
	case WTP_STRUCTURE:
	    w_get_arity(&arity, v);
	    w_get_functor(&functor, v);
	    if (functor == TK_RIF && arity == 2) {
		PWord arg;
		int   argt;

		push_rator(hpe, 0, 0);	/* number of vars not known yet */
		w_get_argn(&arg, &argt, v, 1);

		*pst_rand++ = expand_pred(hpe, CVTWALK(arg, argt));

		w_get_argn(&arg, &argt, v, 2);
		rxpbod(hpe, CVTWALK(arg, argt));

		bld_clause(hpe);
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
	    (void) force_uia(hpe, &v, &t);
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
cvt_walk_term(PE, PWord v)
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
cvt_walk_list(PE, PWord v)
{
    PWord carv, cdrv;
    int   cart, cdrt;

    w_get_car(&carv, &cart, v);
    w_get_cdr(&cdrv, &cdrt, v);

    return (MK_LIST(CVTWALK(carv, cart), CVTWALK(cdrv, cdrt)));
}

static pword
cvt_walk_sym(PE, PWord v)
{
    return (MK_SYM((int) v));
}

static pword
cvt_walk_int(PE, PWord v)
{
    return (MK_INT(v));
}


static pword
cvt_walk_uia(PE, PWord v)
{
    char  buf[512];

    w_get_uianame((UCHAR *)buf, v, 512);

    return (MK_UIA(buf));
}


#ifdef DoubleType
static pword
cvt_walk_dbl(PE, PWord  v)
{
    double d;

    w_get_double(&d, v);
    return (MK_DOUBLE(d));
}
#endif

static pword
cvt_walk_var(PE, PWord v)
{
    int   i;

    for (i = 0; i < vtp && evtable[i] != v; i++) ;

    if (i >= vtp) {
	i = vtp++;
	evtable[i] = v;
    }

    return (MK_VO(i));
}
#endif /* XPANDTM */
