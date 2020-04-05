/*===============================================================*
 *			varproc.c            
 *		Copyright (c) 1985 by Kevin A. Buettner
 *		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 *
 |			-- functions for dealing with variables prior to compilation.
 |
 * Author:  Kevin A. Buettner
 * Creation: 6/16/85
 * Revision History:
 *===============================================================*/
#include "defs.h"
#include "varproc.h"

/*
 * How far from where the E register points that the permanent variables
 * reside.
 */

#define PERMOFFSET	1

varinf vtbl[VTBLSIZE + 2];	/* variable table               */

int   call_env_sizes[MAXGLS];

static	void	init_vtbl	( int );
static	void	vwalk_term	( pword, int, int );
static	void	vwalk_const	( pword, int, int );
static	void	vwalk_list	( pword, int, int );
static	void	vwalk_rule	( pword, int, int );
static	void	vwalk_vo	( pword, int, int );

static void
init_vtbl(n)
    int   n;
{
    register int i;

    for (i = 0; i < n; i++) {
	vtbl[i].firstocc = -1;
	vtbl[i].lastocc = -1;
	vtbl[i].istoplevinhead = 0;
	vtbl[i].noccurences = 0;
	vtbl[i].pvnum = 0;

	/* assume unsafe...this is only used to determine whether or not to
	 * use unify_value or unify_local_value
	 */

	vtbl[i].unsafe = 1;
	vtbl[i].home = 0;
	vtbl[i].usecnt = 0;
	vtbl[i].target = 0;

    }
}

#define vwalk(t) vwalk_rule(t,-1,-1)

static void (*vwalk_tbl[]) ( pword, int, int ) = {
    vwalk_term,
	vwalk_const,
	vwalk_list,
	vwalk_const,
	vwalk_rule,
	vwalk_vo,
	vwalk_const,
	vwalk_const,
	vwalk_const,
	vwalk_const,
	vwalk_const,
	vwalk_const,
	vwalk_const
};

#define VWALK(t,l,g) (*vwalk_tbl[TYPEOF(t)])(t,l,g)

static void
vwalk_term(t, l, gn)
    pword t;
    int   l, gn;
{
    register int i, n;

    n = TERM_ARITY(t);
    for (i = 1; i <= n; i++)
	VWALK(TERM_ARGN(t, i), l + 1, gn);
}

static void
vwalk_const(t, l, gn)
    pword t;
    int   l, gn;
{
    /* empty */
}

static void
vwalk_list(t, l, gn)
    pword t;
    int   l, gn;
{
    while (TYPEOF(t) == TP_LIST) {
	VWALK(LIST_CAR(t), l + 1, gn);	/* Walk the car */
	t = LIST_CDR(t);	/* follow the cdr */
    }
    VWALK(t, l + 1, gn);	/* walk the final cdr */
}

static void
vwalk_rule(t, l, gn)
    pword t;
    int   l, gn;
{
    register int i, n;

    if (gn != -1)
	printf("Something funny is happening in vwalk_rule.\n");
    else {
	VWALK(RULE_HEAD(t), -1, 0);
	n = (int) RULE_NGOALS(t);
	for (i = 1; i <= n; i++) {
	    VWALK(RULE_GOALN(t, i), -1, i);
	}
    }
}

static void
vwalk_vo(t, l, gn)		/* the payoff */
    pword t;
    int   l, gn;
{
    register int vo = (int) VO_VAL(t);

    if (vtbl[vo].firstocc == -1) {
	vtbl[vo].firstocc = gn;
	vtbl[vo].noccurences = 0;
    }

    vtbl[vo].istoplevinhead |= ((l == 0) && (gn == 0));
    vtbl[vo].lastocc = gn;
    vtbl[vo].noccurences++;
}

#define ISTEMP(fo,lo,istoplevinhead,ng,fgn) \
        ((fo == 0 && lo <= fgn) || (fo == lo) || (istoplevinhead))


/*
 * classify_vars is given a clause.  It initializes and fills in vtbl
 * for the clause and returns the number of permanent variables found.
 */

int
classify_vars(t)
    pword t;
{
    int   ptb[VTBLSIZE];
    register int i, j;		/* indices */
    register int temp;		/* swapping temporary   */
    int   nv;			/* number of variables  */
#ifndef SlowCut
    int   ng;			/* number of goals      */
#endif
    int   fgn,			/* first goal number (won't be 1 when ! comes
				 * first)
				 */
          npv;			/* number of permanent variables */

    nv = RULE_NVARS(t);		/* get number of variables */
#ifndef SlowCut
    ng = RULE_NGOALS(t);	/* and the number of goals */
#endif

    init_vtbl(nv);		/* initialize the variable table */

    vwalk(t);			/* get first occurence, last occurence, and
				 * in structure information about the
				 * variables in the clause
				 */

#ifdef SlowCut
    fgn = 1;			/* first goal number always one with SlowCut */
#else  /* SlowCut */
    /* find number of first goal that isn't a cut */
    for (fgn = 1;
	 fgn <= ng && TYPEOF(RULE_GOALN(t, fgn)) == TP_SYM &&
	 FUNCTOR_TOKID(RULE_GOALN(t, fgn)) == TK_CUT;
	 fgn++) ;
#endif /* SlowCut */

#ifdef NewMath
    /* temps occurring in a first arithmetic goal must be considered perms */
    if (fgn <= ng && isarithmetic(RULE_GOALN(t, fgn)))
	fgn = 0;
#endif

    /* record indices of permanent variables and increment the perm var count
     */

    npv = 0;			/* haven't found any permanent vars yet */
    for (i = 0; i < nv; i++)
	if (!ISTEMP(vtbl[i].firstocc,
		    vtbl[i].lastocc,
		    vtbl[i].istoplevinhead, ng, fgn))
	    ptb[npv++] = i;

    /* We now wish to sort the indices in order of last used.  The ptb
     * index will then correspond to the permanent variable
     * number.
     */

    for (i = 0; i < npv - 1; i++)
	for (j = i + 1; j < npv; j++)
	    if (vtbl[ptb[i]].lastocc < vtbl[ptb[j]].lastocc) {
		temp = ptb[i];	/* swap */
		ptb[i] = ptb[j];
		ptb[j] = temp;
	    }

    /* add offset to get to first var (otherwise we point off of where
     * the E register points...these will be negative displacements
     * later on)
     */

    for (i = 0; i < npv; i++)
	vtbl[ptb[i]].pvnum = i + PERMOFFSET;

    return (npv);
}

/*
 * Calculate the sizes of the environment for each call. This is so we
 * know how to do a trim after coming back, since we'll know how many we still
 * need. The size includes things like permanent variables and the like.
 */

void
compute_call_env_sizes(ngoals, nvars)
    int   ngoals;
    int   nvars;
{
    register int i;
    register int j;

    /* Clear all of the sizes */
    for (j = 0; j <= ngoals; j++)
	call_env_sizes[j] = 0;

    /* For each call, we must keep all of the permament variables needed for
     * each following call.
     */
    for (i = 0; i < nvars; i++)
	if (vtbl[i].pvnum)	/* if it is a permanent var */
	    /* Make sure in environment for each previous call */
	    for (j = vtbl[i].lastocc - 1; j > 0; j--)
		call_env_sizes[j]++;
}
