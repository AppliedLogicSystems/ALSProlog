/*=============================================================*
 |			parser.c             
 |		Copyright (c) 1985-1995 by Kevin A. Buettner
 |
 |			-- parser for prolog system
 |
 | Author:  Kevin A. Buettner
 | Creation: 10/25/84  -- MV-8000 version
 | Revision History:
 | 06/15/85 - K. Buettner -- compiler mods, port to Unix
 | 08/08/85 - K. Buettner -- Split lexical analyzer to separate file;
 | 01/12/86 - K. Buettner -- port to IBM-PC for ALS
 | 10/26/94 - C. Houpt	-- Various UCHAR* casts.
 *=============================================================*/
#include "defs.h"


#include <setjmp.h>
#include "lexan.h"
#include "parsstak.h"
#include "module.h"
#include "icodegen.h"

/*
 * Global Variables
 */

/*
 * prs_erc is an array indexed by prs_erc_index.  It contains
 *      the address/stack information required by longjmp to get back to the
 *      most current invokation of the parser.  We need a stack of jmp_buf's
 *      because the parser may be invoked recursively via consult commands in
 *      a file being read.
 */

#ifdef KERNAL
static jmp_buf prs_erc[10];	/* buffers for error recovery   */
#else
static jmp_buf prs_erc[30];	/* buffers for error recovery   */
#endif /* KERNAL */
static int   prs_erc_index = -1; /* index into the above         */

/*
 * vtable is an array of UIA's which represent the variable names
 *
 */

#ifdef KERNAL
static pword vtable[64];	/* variable name table          */
#else
static pword vtable[240];	/* variable name table          */
#endif /* KERNAL */

static int   nxtvar;		/* next free space in var table */


/*
 * The parser uses two stacks: an operator stack represented by
 * pst_rator and ps_rator; and an operand stack represented by
 * pst_rand and ps_rand.  The ps_ arguments are the actual allocated
 * storage.  The pst_ arguments are pointers to the tops.
 *
 * The parser stacks will grow from low to high memory with the tops of stacks
 * pointing one beyond where the top element resides.
 */

struct rator *pst_rator;	/* parser stack top -- rator */
pword *pst_rand;		/* parser stack top -- rand */

static struct rator ps_rator[PSTKSZ];
static pword ps_rand[PSTKSZ];

/*
 * errcount is the number of errors encountered so far in the parse.  Care
 *      must be taken to preserve this value in recursive consults.
 */

int   errcount = 0;

static	void	bld_struct	( void );
static	int	reduce_stack	( int );
static	void	push_op		( int );
static	void	nt_term0	( void );
static	void	nt_term		( int );
static	void	nt_list		( void );
static	void	nt_listexpr	( void );
static	void	nt_args		( void );
static	void	nt_uselist	( void );
static	void	nt_exportlist	( void );
static	void	check_sym	( const char * );
static	void	nt_toplevel	( void );
static	int	bottomRead	( const char *, const char * );
static	void	buf_nextline	( lxi_but * );
static	int	buf_syntaxerror	( lxi_but *, const char * );


/*
 * parser_init is used to initialize the parser.
 */

void
parser_init(void)
{
    symtab_init();
    parser_reset();		/* Reset the parser */
}

/*
 * parser_reset resets the parser.
 */

void
parser_reset(void)
{
    nxtvar = 0;
    pst_rator = ps_rator;	/* initialize tops of stacks */
    pst_rand = ps_rand;
}


/*
 * find_var is given a variable name and returns the index of the
 *   variable in the variable name table, adding it if necessary.
 *   '_' is always given a new index, no matter how many times it occurs
 *
 */

int
find_var(char *s)
{
    register int i;

    if (*s == '_' && *(s + 1) == '\0')
	i = nxtvar;
    else
	for (i = 0; i < nxtvar && strcmp(UIA_NAME(vtable[i]), s) != 0; i++) ;

    if (i == nxtvar) {
	vtable[i] = MK_UIA(s);
	nxtvar++;
    }

    return (i);			/* return variable index */
}



/*
 * parsing functions are prefixed by an nt_
 */

#define TOKENIS(t) ((curtkty == TKTP_OTHER || curtkty == TKTP_FUNCTOR ||\
                      curtkty == TKTP_OP) && curtok == (t))

#define PRE_OP  (curtkty == TKTP_OP && (TOKUNOP(curtok) & ASSC_PREFIX))

#define INFIXOP ((curtkty == TKTP_OP || curtkty == TKTP_FUNCTOR) && \
         TOKBINOP(curtok))

#define POST_OP (curtkty == TKTP_OP && TOKUNOP(curtok) && \
	!(TOKUNOP(curtok) & ASSC_PREFIX))

/* HIPREC is defined to be bigger than 1200<<4 */
#define HIPREC 20000

/*
 * push_rator pushs and operator onto the operator stack
 */

void
push_rator(long tokid, long prec)
{
    pst_rator->last_rand = pst_rand;
    pst_rator->precedence = prec;
    pst_rator->token_id = tokid;
    pst_rator++;
}

/*
 * bld_struct takes the top operator on the operator stack and builds a
 * structure with it and the operands on the operand stack.
 */

static void
bld_struct(void)
{
    int   n;

    pst_rator--;		/* pop the operator stack */

    n = pst_rand - pst_rator->last_rand;	/* compute arity */

    if (n < 0)
	parser_error("Error in bld_struct.");
    else if (n == 0)
	*pst_rand++ = MK_SYM(pst_rator->token_id);
    else if (n == 2 && pst_rator->token_id == TK_DOT) {
	/* Handle alternate list notation.  */
	*(pst_rand - 2) = MK_LIST(*(pst_rand - 2), *(pst_rand - 1));
	pst_rand--;
    }
    else {
	pword t = MK_TERM(n);

	TERM_FUNCTOR(t) = MK_FUNCTOR(pst_rator->token_id, n);

	for (; n > 0; n--)
	    TERM_ARGN(t, n) = *--pst_rand;

	*pst_rand++ = t;
    }
}

/*
 * bld_clause is similar to bld_struct, but is responsible for building
 *      a structure that the compiler knows how to handle rather than a
 *      structured term.
 */

void
bld_clause(void)
{
    pword r;
    int   n;

    pst_rator--;		/* pop operator stack */
    n = pst_rand - pst_rator->last_rand - 1;	/* compute number of goals */

#ifdef KERNAL
    /* I really don't think this is a parser error - it looks like a
       low-level fatal error. */
    if (n < 0) {
    	fprintf(stderr, "Error in bld_clause.");
    	exit(-1);
    }
#else
    if (n < 0)
	parser_error("Error in bld_clause.");
#endif /* KERNAL */

    r = MK_RULE(n);

    for (; n > 0; n--)
	RULE_GOALN(r, n) = *--pst_rand;

    RULE_NVARS(r) = pst_rator->token_id;	/* token id doubles as nvars */
    RULE_HEAD(r) = *--pst_rand;

    *pst_rand++ = r;
}


/*
 * reduce_stack(prec) is responsible for making structures out of operator
 *      operand combinations on the stack.  Structures are made until
 *      a operator is found with precedence greater than prec.
 */

static int
reduce_stack(int prec)
{
    int   arity;

    arity = pst_rand - TOP_RATOR.last_rand;

    while (PREC_ONLY(TOP_RATOR.precedence) < PREC_LEFT(prec)) {

	/*
	 * If we have a - operator and a single numeric argument, then
	 * convert it directly;  if we have anything else, we must call
	 * bld_struct.
	 */

	if (arity == 1 && TOP_RATOR.token_id == TK_MINUS) {
	    double d;

	    if (TYPEOF(TOP_RAND) == TP_INT) {
		long  iv = INT_VAL(TOP_RAND);

		if (iv == MINPROLOGINT)
		    TOP_RAND = MK_DOUBLE((double) -iv);
		else
		    TOP_RAND = MK_INT(-iv);
	    }
	    else if (is_double(&d, TOP_RAND))
		TOP_RAND = MK_DOUBLE(-d);
	    else
		goto call_bld_struct;

	    pst_rator--;
	}
	else {

call_bld_struct:

	    /*
	     * Before calling bld_struct, we make sure that the arity matches
	     *    the associativity declarations.  Hamid noticed that the
	     *    parser would take things like:
	     *             f :- g;.
	     *    and rap the g up in a semicolon (which is clearly wrong).
	     */

	    if (arity == 1 && !(TOKUNOP(TOP_RATOR.token_id)))
		parser_error("Right hand term expected for binary operator");

	    bld_struct();
	}
	arity = pst_rand - TOP_RATOR.last_rand;
    }

    return PREC_RIGHT(TOP_RATOR.precedence) > PREC_ONLY(prec);
}



/*
 * push_op is used to push binary operators and postfix operators; both of
 * which already have had an operand pushed onto the operand stack.  Thus
 * we need to set the last_rand field somewhat differently than in push_rator
 * above.
 */

static void
push_op(int raw_prec)
{
    pst_rator->last_rand = pst_rand - 1;
    pst_rator->precedence = raw_prec;
    pst_rator->token_id = curtok;
    pst_rator++;
}


/*
 * nt_term0 is responsible for parsing terms whose precedence is 0.  This
 *      includes constants, numbers, lists, and structured terms.   Anything
 *      that has operators in it goes through nt_term which knows about the
 *      operators.
 */

static void
nt_term0(void)
{
    long  tok;

    if (curtkty == TKTP_VAR) {
	*pst_rand++ = MK_VO(curtok);
	next_token();
    }
    else if (curtkty == TKTP_INT) {
	*pst_rand++ = MK_INT(curtok);
	next_token();
    }
    else if (curtkty == TKTP_OTHER) {
	if (curtok == TK_LBRAC)
	    nt_list();
	else if (curtok == TK_LPAREN) {
	    next_token();
	    nt_term(1200);
	    if (curtkty == TKTP_OTHER && curtok == TK_RPAREN)
		next_token();
	    else
		parser_error("')' expected.");
	}
	else if (curtok == TK_LCURLY) {
	    push_rator(TK_CURLYS, 0);
	    next_token();	/* consume the { */
	    if (curtkty != TKTP_OTHER || curtok != TK_RCURLY)
		nt_term(1200);
	    if (curtkty != TKTP_OTHER || curtok != TK_RCURLY)
		parser_error("'}' expected.");

	    next_token();	/* consume the right curly */
	    bld_struct();	/* build the curly structure */
	}
	else {			/* must be a constant */
	    goto makeconst;
	}
    }
    else if (curtkty == TKTP_STRING) {
	*pst_rand++ = bld_strl(tokstr);
	next_token();
    }
    else if (curtkty == TKTP_FUNCTOR) {
	push_rator(curtok, 0);
	next_token();		/* get the paren     */
	next_token();		/* consume the paren */
	nt_args();		/* get the arguments */
    }
    else if (curtkty == TKTP_OP) {
	/* operator found in non-op position, turn it into a constant */
	*pst_rand++ = MK_SYM(curtok);
	next_token();
    }
    else if (curtkty == TKTP_CONST) {
makeconst:
	tok = curtok;
	next_token();
	if (curtkty == TKTP_OTHER && curtok == TK_LPAREN) {
	    next_token();	/* consume the left paren */
	    push_rator(tok, 0);
	    nt_args();
	}
	else
	    *pst_rand++ = MK_SYM(tok);
    }
    else if (curtkty == TKTP_OBJECT) {
	*pst_rand++ = (pword) curtok;
	next_token();
    }
}



/*
 * nt_term(n) is responsible for parsing a term whose precedence is no
 *      greater than n.
 *
 *      The parser stack is instrumental in making things go smoothly.  We
 *      start of by pushing a "fence" on the parser stack with value
 *      HIPREC.  This fence will not be crossed and will be removed only
 *      after a term has been built.
 *
 *      We start out by scanning for prefix operators.  Each prefix operator
 *      must have prec less than or equal to prefix_prec which is initially n.
 *      We consume prefix operators from the input and push them on the
 *      stack with a fence whose value is whatever the the precedence of
 *      the prefix operator is.  Additionally, we set prefix_prec to the
 *      precedence of this operator (with and adjustment if the associativity
 *      is fx).  This means that all immediately following prefix operators
 *      must have precedence less or equal to the operator just scanned
 *      or an error is signalled.  prefix_prec is also set immediately after
 *      reading an infix operator.
 *
 *      After reading the prefix operators, we should be left with a term
 *      with no operators. (See nt_term0).  But it is possible that
 *      there is punctuation or an infix operator as in:
 *              [-,+]
 *      or
 *              - + -
 *      In the first case, we want both the '-' and '+' to be treated
 *      as symbols rather than prefix operators.
 *      The second case isn't as clear cut, but there are strong arguments
 *      for making it parse as '+'('-','-').
 *
 */

static void
nt_term(int n)
{
    int   prefix_prec;


    push_rator(0, HIPREC);	/* push the "fence" on */
    n <<= 4;			/* force n into prefix/assoc format */
    prefix_prec = n | ASSC_RIGHT;

    for (;;) {
	if (PRE_OP) {
	    while (PRE_OP && PREC_ONLY(TOKUNOP(curtok)) < prefix_prec) {
		prefix_prec = PREC_RIGHT(TOKUNOP(curtok));
		push_rator(curtok, TOKUNOP(curtok));
		next_token();
	    }

	    if (INFIXOP) {
		/* turn prev prefix op into a symbol */
		bld_struct();
		goto post_op;
	    }
	}

	if (curtkty != TKTP_OTHER ||
	    (curtok != TK_RPAREN && curtok != TK_RBRAC && curtok != TK_RCURLY
	     && curtok != TK_VBAR))
	    nt_term0();

post_op:

	while (POST_OP && PREC_ONLY(TOKUNOP(curtok)) <= n) {
	    if (!reduce_stack(TOKUNOP(curtok)))
		parser_error(
		     "Associativity conflict to left of postfix operator.");

	    push_op(TOKUNOP(curtok));
	    next_token();
	}

	if (INFIXOP && PREC_ONLY(TOKBINOP(curtok)) <= n) {
	    if (!reduce_stack(TOKBINOP(curtok)))
		parser_error(
		       "Associativity conflict to left of infix operator.");

	    prefix_prec = PREC_RIGHT(TOKBINOP(curtok));
	    push_op(TOKBINOP(curtok));
	    next_token();
	}
	else
	    break;
    }

    (void) reduce_stack(HIPREC);

    pst_rator--;		/* pop of the "fence" */
    if (pst_rator->last_rand == pst_rand)
	parser_error("Non-empty term expected.");
}


/*
 * nt_list is responsible for parsing lists (in the form [a,b,c | T] etc).
 */

static void
nt_list(void)
{
    if (curtkty == TKTP_OTHER && curtok == TK_LBRAC)
	next_token();
    else
	parser_error("'[' expected.");

    if (curtkty == TKTP_OTHER && curtok == TK_RBRAC) {
	next_token();
	*pst_rand++ = MK_SYM(TK_NIL);
    }
    else {
	nt_listexpr();

	if (curtkty == TKTP_OTHER && curtok == TK_RBRAC)
	    next_token();
	else
	    parser_error("']' expected.");
    }
}


static void
nt_listexpr(void)
{
    pword temp;

    nt_term(999);

    if (TOKENIS(TK_COMMA)) {
	next_token();

	if (curtkty == TKTP_OTHER &&
	    (curtok == TK_RPAREN || curtok == TK_RBRAC ||
	     curtok == TK_RCURLY || curtok == TK_VBAR))
	    parser_error("Term expected after comma in list.");

	nt_listexpr();
    }
    else if (TOKENIS(TK_VBAR)) {
	next_token();
	nt_term(999);
    }
    else
	*pst_rand++ = MK_SYM(TK_NIL);

    temp = *--pst_rand;
    TOP_RAND = MK_LIST(TOP_RAND, temp);
}

static void
nt_args(void)
{
/* Special metrowerks 4 bug.  If the second call to nt_term uses 999
   instead of i, compiler crashes  
*/
	int i = 999;
    nt_term(999);

    while (!(curtkty == TKTP_OTHER && curtok == TK_RPAREN)) {
	if (TOKENIS(TK_COMMA))
	    next_token();
	else
	    parser_error(	/* Need comma or right paren  */
			 "Comma or right paren expected in argument list.");

	if (curtkty == TKTP_OTHER &&
	    (curtok == TK_RPAREN || curtok == TK_RBRAC ||
	     curtok == TK_RCURLY || curtok == TK_VBAR))
	    parser_error("Term expected after comma in argument list.");

	nt_term(i);
    }

    if (curtkty != TKTP_OTHER || curtok != TK_RPAREN)
	parser_error("')' expected.");

    next_token();		/* consume the )     */

    bld_struct();
}

#define adduse(tok)	icode(IC_ADDUSE,tok,0,0,0)
#define end_module()	icode(IC_ENDMODULE,0,0,0,0)
#define new_module(tok)	icode(IC_NEWMODULE,tok,0,0,0)
#define export_predicate(tok,arity)	icode(IC_EXPORTPRED,tok,arity,0,0)

static void
nt_uselist(void)
{
    for (;;) {
	check_sym("Module name expected in use declaration.");

	adduse(curtok);

	next_token();
	if (curtkty == TKTP_FULLSTOP)
	    return;
	else if (TOKENIS(TK_COMMA))
	    next_token();
	else
	    parser_error("'.' or ',' expected in use declaration.");
    }
}

static void
nt_exportlist(void)
{
    long  token;
    int   arity;

    while (1) {
	check_sym("Predicate name expected in export declaration.");

	token = curtok;

	next_token();
	if (!TOKENIS(TK_SLASH))
	    parser_error("'/' expected after predicate name in export list.");

	next_token();
	if (curtkty != TKTP_INT)
	    parser_error("Integer expected after '/' in export list.");

	arity = curtok;

	next_token();

	export_predicate(token, arity);		/* icode call */

	if (curtkty == TKTP_FULLSTOP)
	    return;
	else if (TOKENIS(TK_COMMA))
	    next_token();
	else
	    parser_error("'.' or ',' expected in export list.");
    }
}


static void
check_sym(const char *errstr)
{
    if (curtkty == TKTP_OTHER || curtkty == TKTP_OP || curtkty == TKTP_CONST)
	return;

    if (curtkty == TKTP_OBJECT && TYPEOF((pword) curtok) == TP_UIA) {
	curtok = find_token((UCHAR *)UIA_NAME((pword) curtok));
	curtkty = TKTP_CONST;

	return;
    }

    parser_error(errstr);
}



static void
nt_toplevel(void)
{

    if (curtkty == TKTP_FULLSTOP)
	return;
    else if (TOKENIS(TK_MODULE)) {
	next_token();
	check_sym("Module name expected in module declaration.");
	new_module(curtok);
	next_token();
    }
    else if (TOKENIS(TK_USE)) {
	next_token();
	nt_uselist();
    }
    else if (TOKENIS(TK_ENDMOD)) {
	next_token();
	end_module();
    }
    else if (TOKENIS(TK_EXPORT)) {
	next_token();
	nt_exportlist();
    }
    else {
	nt_term(1200);

	if (curtkty != TKTP_FULLSTOP)
	    parser_error("'.' expected.");

	parser_action(nxtvar, TOP_RAND);

	return;
    }

    if (curtkty != TKTP_FULLSTOP)
	parser_error("'.' expected.");
}

void
nt_query(void)
{
    if (curtkty == TKTP_FULLSTOP)
	return;

    push_rator(TK_QUEST, 0);	/* use ?- as fake head */
    nt_term(1200);
    bld_struct();

    if (curtkty != TKTP_FULLSTOP)
	parser_error("'.' expected.");

    parser_action(nxtvar, TOP_RAND);
}



void
parser_error(const char *errstring)
{
    errcount++;

    /* Do any error recovery specified by the type of file */
    (void) ((*(lexbdp->err_rec)) (lexbdp, errstring));

    longjmp(prs_erc[prs_erc_index], 1);
}

void
read_loop(void  (*rdfunc) ( void ), int pptidx)
{
    int   eof = 0;

    prs_erc_index++;

    while (!eof) {
	setjmp(prs_erc[prs_erc_index]);

	eof = 0;
	parser_reset();
	alc_rst();

	curprompt = prompts[pptidx].pprompt;	/* set primary prompt */
	next_token();
	curprompt = prompts[pptidx].sprompt;	/* set secondary prompt */

	if (curtkty == TKTP_EOF) {
	    rewind(stdin);
	    eof = 1;
	}
	else
	    (*rdfunc) ();
    }

    prs_erc_index--;
}


pword
bld_strl(char *s)
{
    register char *t = s;
    register pword r = MK_SYM(TK_NIL);

    while (*t)
	t++;			/* point to end */

    while (t != s) {
	t--;
	r = MK_LIST(MK_INT(*t), r);
    }

    return (r);
}

pword
bld_vlst(void)
{
    int   i = nxtvar - 1;
    pword r = MK_SYM(TK_NIL);

    while (i >= 0) {
	r = MK_LIST(vtable[i--], r);
    }
    return (r);
}

int
qtok(int t)
{
    register UCHAR *s = TOKNAME(t);

    if (t == TK_NIL || t == TK_CUT)
	return (0);		/* special cases */

    if (lx_chtb[(*s)] != LX_LCAL)
	return (1);

    s++;

    while (*s && (lx_chtb[(*s)] == LX_LCAL || lx_chtb[(*s)] == LX_UCAL
		  || lx_chtb[(*s)] == LX_NUM))
	s++;

    return (*s);
}


void
bld_showanswers(void)
{
    register pword vl;
    register int i;

    push_rator(find_token((UCHAR *)"showanswers"), 0);
    *pst_rand++ = bld_vlst();

    for (vl = MK_SYM(TK_NIL), i = nxtvar - 1; i >= 0; i--)
	vl = MK_LIST(MK_VO(i), vl);

    *pst_rand++ = vl;
    bld_struct();
}


/*
 * consult will return -1 if file cannot be opened.  Otherwise, consult
 * returns the number of syntax errors encountered while parsing the file.
 */

int
consult(int f)
{
    int   ec;
    int   oldf = fio_seeing();

    parser_reset();

    if (fio_see(f)) {
	read_loop(nt_toplevel, PMPT_CONSULT);
	ec = errcount;
	fio_seen();
	fio_see(oldf);

#ifdef Indexing
	gen_indexing();
#endif

	w_relinkall();

	return ec;
    }
    else
	return 0;
}

/* This is what actually does a read */
static int
bottomRead(const char *pprompt, const char *sprompt)
{
    int   retval;

    prs_erc_index++;		/* advance the parser recovery index */

    /* make the parser return here on an error */
    setjmp(prs_erc[prs_erc_index]);

    /* Initialize everything */
    parser_reset();
    alc_rst();
    curprompt = pprompt;	/* Start with the primary prompt */

    next_token();		/* prime the pump */

    if (curtkty == TKTP_EOF) {
	retval = 0;		/* EOF has been hit */

	if (curfd == stdin) {

	    rewind(stdin);

	    clearerr(curfd);

	    /* force new buffer to be gotten */
	    lexbdp->curpos = (char *) 0;
	}
    }
    else {
	curprompt = sprompt;	/* Switch to secondary prompt */

	/* read the term that the user typed in */
	nt_term(HIPREC);

	/* error if he didn't type a .  */
	if (curtkty != TKTP_FULLSTOP)
	    parser_error("'.' expected.");

	if (pst_rand == ps_rand)
	    parser_error("Empty term unacceptable in read.\n");

	/* Got a non-eof */
	retval = 1;
    }

    prs_erc_index--;

    return retval;
}


/*
 * prim_read is called by pbi_read.
 *
 * It returns a cons cell whose car is the structure that the parser builds.
 * The cdr is a list of variable names occuring in the structure. To use either
 * of these components effectively, they will have to be copied onto the heap.
 *
 */

pword
prim_read(void)
{
    /* Do read and check for EOF */
    if (bottomRead(prompts[PMPT_READ].pprompt, prompts[PMPT_READ].sprompt))
	return (MK_LIST(*--pst_rand, bld_vlst()));
    else			/* EOF has been hit */
	return (MK_LIST(
			   MK_SYM(find_token((UCHAR *)"end_of_file")),
			   MK_SYM(find_token((UCHAR *)"[]"))
		));
}


#define whitespace(c) ((c) <= 32 || (c) == 127)

static void
buf_nextline(lxi_but *lbp)
{
    register char *bp = lbp->bufptr;

    if (*bp) {
	while (*++bp && *bp != '\n') ;

	if (*bp && whitespace(*bp))
	    bp++;

	lbp->bufptr = bp;
	lbp->curpos = bp;
    }
}


static int
buf_syntaxerror(lxi_but *lbp, const char *errstring)
{
    return (0);
}

int
exec_query_from_buf(char *buf)
{
    lxi_but *oldlb;
    lxi_but lb;

    oldlb = lexbdp;
    lexbdp = &lb;

    lb.bufptr = buf;
    lb.curpos = buf;
    lb.nextbuf = buf_nextline;
    lb.err_rec = buf_syntaxerror;
    lb.see_idx = -1;

    prs_erc_index++;
    if (setjmp(prs_erc[prs_erc_index])) {
	prs_erc_index--;
	lexbdp = oldlb;
	return 0;
    }
    else {
	parser_reset();
	alc_rst();
	push_rator(TK_RIF, 0);
	next_token();
	nt_term(1200);
	bld_struct();
	prs_erc_index--;
	lexbdp = oldlb;
	parser_action(nxtvar, *--pst_rand);
	return 1;
    }
}

/*
 * token_name is called by assembly language routines that wish to convert
 * tokens into strings
 */

UCHAR *
token_name(int tok)
{
    return (TOKNAME(tok));
}
