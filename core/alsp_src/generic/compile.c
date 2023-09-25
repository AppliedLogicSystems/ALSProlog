/*===================================================================*
 |			compile.c            
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |			-- clause compilation module for prolog compiler
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/16/85
 | Revision History:
 | 02/11/87 - Kevin     -- Native code mods
 | 10/26/90 - Kevin     -- compiler directive stuff added
 | 11/21/94 - C. Houpt	-- Allocate large global arrays with malloc() for
 |						   Mac compilers that can't handle them.
 *===================================================================*/
#include "defs.h"
#include "varproc.h"
#include "cutmacro.h"
#include "compile.h"
#include "module.h"
#include "icode.h"
#include "icodegen.h"

static int npv;				/* number of permanent variables */
static int ngoals;			/* number of goals               */
static int nvars;			/* number of variables           */
static int goalnumber;		/* which goal we're working on,  *
				 			 * 0 represents the head         */
static int nargs;		    /* number of arguments in the current goal */

static int firstgoalnumber;
			/* usually 1, but not when there are cut(s) preceding
			 * the first real goal
			 */

static int nargsmatched;
			/* number of arguments matched in head so far.
			 */

static int to_do[TODOSIZE];

		/*-----------------------------------------------*
		 * the "to_do" stack.  We use this stack when
		 * compiling structure to keep track of register
		 * numbers allocated by previous recursive calls
		 * which built other structure or need other
		 * structure built (it differs between the head
		 * and the body).  Head structure is built top-
		 * down and structure occurring in the body is
		 * built bottom-up
		 *-----------------------------------------------*/

static int to_do_top = 0;
		/*----------------------------------------------*
		 * points to the next available location on the
		 * "to do" stack
		 *----------------------------------------------*/

#define TDT_SET(tdt,n) tdt = to_do_top; to_do_top += n
#define TDT_RESET(tdt) to_do_top = tdt

static pword model[MODELSIZE];

		/*----------------------------------------------*
		 * stack model of argument blocks and environment.
		 * The following variables are indices into this
		 * block.
		 *----------------------------------------------*/

static int model_Adst;		/* index of destination arguments */
static int model_Adst2;		/* index of destination arguments which might
				 			 * overlap the source arguments */

static int model_Eend;		/* end of safe part of E */
static int model_SPstart;

		/*----------------------------------------------*
		 * Place where SP starts out at before the compilation
		 *      of a goal allocates some temps and moves it.
		 *      This value is used for the allocation of
		 *      temporaries.  Between model_SP and model_SPstart
		 *      are the potentially free temporaries.
		 *----------------------------------------------*/

	/*==================================*
	 * Compiler directive variables
	 *==================================*/

static int cd_cutpt;	/* Variable number to put cutpt in; -1 if not needed */
static int cd_cmod;		/* Clause module									 */
static int cd_gmod;		/* Goal module; i.e, module to place next goal in	 */

static int model_SP;
static int model_E;

static	int		comp_rule				( pword );
static	void	deallocate_environment	( pword, int, int, int );
static	void	initialize_environment_variables ( void );
#if defined(InMath) && !defined(NewMath)
static	long	regfreemap				( void );
#endif
#ifdef NewMath
static	long	regusemap				( void );
static	void	arith_temp_homes		( void );
#endif
static	int		do_macro				( pword, int );
static	int		goalsize				( pword );
static	void	incr_usecnt				( int );
static	void	init_model				( pword, pword );
static	void	init_targets			( pword );
static	void	init_only_targets		( pword );
static	void	reassign_homes			( pword );
static	void	comp_head				( pword );
static	void	record_first_argument	( int, pword );
static	void	comp_head_structure		( int );
static	void	comp_hstruct_argument	( pword, int );
static	void	move					( int, int );
static	int		find_home				( int );
static	int		find_temp_in_reg		( int );
static	int		free_target				( int );
static	void	move_perms				( int );
static	void	move_to_temp			( int );
static	void	comp_goal				( pword );
static	int		comp_goal_structure		( pword, int );
static	int		comp_struct_arg1		( pword );
static	void	comp_struct_arg2		( pword, int, int );
static	pword	get_compiler_directives	( pword );
static	void	compiler_directives 	( void );
static	void	emit_cd_cutpt			( void );

#if 0
#ifdef MaxFunc
/*
 * max returns the greater of its two parameters
 * 	-- We are using a macro for "max" -- Ilyas, Raman 5/17/90 
 */

int
max(x, y)
    int   x, y;
{
    return (x < y) ? y : x;
}
#else  /* MaxFunc */
#define max(a,b) ((a)<(b) ? (b) : (a))
#endif /* MaxFunc */
#endif

/*=====================================================================*
 * compile_clause takes a rule r generated by parser and generates
 * code for the clause.
 *
 * The compiled code can be found in the icode buffer (see instrs.c).
 *=====================================================================*/

int
compile_clause(pword r, int fromparser)
    /* fromparser: flag indicating if we came from the parser */
{
    pword rh;			/* rule head */

    rh = RULE_HEAD(r);
    if (TYPEOF(rh) == TP_TERM)
	rh = TERM_FUNCTOR(rh);	/* Get the rule head                  */

    if (TYPEOF(rh) != TP_SYM) {	/* See if the rule head of appropriate type */
#ifndef KERNAL
	/* Is this really a parser error? Couldn't it be generated directly? */
	if (fromparser)
	    parser_error("Head of clause of inappropriate type.");
#endif /* KERNAL */
	return 0;		/* return failure code if not   */
    }

    npv = classify_vars(r);	/* Do analysis of the variables and cuts */

    ngoals = RULE_NGOALS(r);	/* Tell everyone how many goals we have  */
    nvars = RULE_NVARS(r);	/* Similarly for the number of variables */

    compute_call_env_sizes(ngoals, nvars);

    /*--------------------------------------------------------------*
	 * Figure out how big of an environment we need after each call...
     * results are left in the call_env_sizes array
     *--------------------------------------------------------------*/

    cd_cmod = cd_gmod = cur_mod;	/* Set the clause module
					 				 * appropriately
					 				 */
    cd_cutpt = -1;		/* No cutpt info requested yet */

    if (!comp_rule(r)) {	/* Generate the code for the clause  */
#ifndef KERNAL
	/* Is this really a parser error? Couldn't it be generated directly? */
	if (fromparser)
	    parser_error("Goal in clause of inapproprate type.");
#endif /* KERNAL */
	return 0;
    }

    /*---------------------------------*
     * and now generate code via icode:
     *---------------------------------*/

    icode(IC_ENDCLAUSE, FUNCTOR_TOKID(rh), FUNCTOR_ARITY(rh), 0, 0);
    return 1;
}

static int
comp_rule(pword rule)
{
    int   gtp;				/* goal type 		*/
    int   macrofix;			/* math fixup flag 	*/
    pword goal;				/* goal under consideration */
    int   functor_id;		/* functor id of the goal   */
    int   garity;			/* arity of goal 	*/
    int   gsize;			/* goal size 		*/
    int   isdeterminateforsure = 0;	/* self descriptive flag */
#ifdef NewMath
    int   lastgoalarithmetic;		/* self descriptive flag */
#endif

    if (TYPEOF(rule) != TP_RULE)
		fatal_error(FE_IN_COMP1, 0);

    ngoals = (int) RULE_NGOALS(rule);

#ifdef NewMath
    /*------------------------------------------------------------------------*
     * If the last goal is arithmetic and we attempt to generate inline math
     * (new math), the stack frame is not set up properly for the inline math.
     * To fake the compiler out, we make it think that there is one more goal.
     * Then the stack frame is set up properly.  This is a hack, but the
     * alternative (modifying init_model and the deallocate code just for
     * handling arithmetic) seemed even uglier.  What's more, the code
     * produced in this fashion is exactly what we want.
     *------------------------------------------------------------------------*/

    if (ngoals && isarithmetic(RULE_GOALN(rule, ngoals))) {
		lastgoalarithmetic = ngoals;	/* set to actual number of goals */
		call_env_sizes[ngoals] = call_env_sizes[ngoals - 1];
		ngoals++;
    }
    else
		lastgoalarithmetic = 0;
#endif

    /*----------------------------------------*
     * Find the first goal that is not a cut.
     *----------------------------------------*/

#ifdef SlowCut
    firstgoalnumber = 1;
#else  /* SlowCut */
    for (firstgoalnumber = 1;
	 	firstgoalnumber <= ngoals &&
	 	TYPEOF(RULE_GOALN(rule, firstgoalnumber)) == TP_SYM &&
	 	FUNCTOR_TOKID(RULE_GOALN(rule, firstgoalnumber)) == TK_CUT;
	 	firstgoalnumber++) ;
#endif /* SlowCut */

    /*------------------------------------------------------------------------*
     * Initialize the model and emit instructions necessary for setting up
     * the environment and stack pointer for the first goal.
     *------------------------------------------------------------------------*/
    goalnumber = 0;

    if (firstgoalnumber > ngoals)
		goal = NIL_VAL;
    else
		goal = get_compiler_directives(RULE_GOALN(rule, firstgoalnumber));

    init_model(RULE_HEAD(rule), goal);

    /*----------------------------------------*
     * Compile the head of the rule.
     *----------------------------------------*/
    comp_head(RULE_HEAD(rule));

    if (firstgoalnumber > 1) {
	/*------------------------------------------------------------------------*
	 * There is at least one cut prior to the first real goal. We want to
	 * emit the instruction which performs the cut and then if the first
	 * goal number is equal to the number of goals, initialize the target
	 * fields in the vtbl array.  This was not done in init_model in order
	 * to prevent the source argument positions from being clobbered.  It
	 * is ok to clobber them after the cut since things are determinate.
	 *------------------------------------------------------------------------*/

	if (ngoals < firstgoalnumber) {
	    icode(I_CUT_PROCEED, EREG, 0, 0, 0);
	    return 1;
	}
	else {
	    icode(I_DOCUT, EREG, 0, 0, 0);
	    if (firstgoalnumber == ngoals)
			init_only_targets(RULE_GOALN(rule, firstgoalnumber));
	}
    }

    /*------------------------------------------------------------------------*
     * firstgoalnumber will always be positive (since we start it out at one
     * in the above loop which sets it).  When ngoals < firstgoalnumber, we
     * have a unit clause and all we need to do is emit some proceed code.
     *------------------------------------------------------------------------*/

    if (ngoals < firstgoalnumber) {
		icode(I_PROCEED, index_of(vtbl[RETIDX].home),
		disp_of(vtbl[RETIDX].home), 0, 0);
		return 1;
    }

    /*------------------------------------------------------------------------*
     * The garbage collector demands that each environment variable
     * has a safe value prior to execution of the first goal. So if we
     * are just prior to the first goal, we initialize all of the
     * uninitialized environment variables.
     *------------------------------------------------------------------------*/

    initialize_environment_variables();

    /*------------------------------------------------------------------------*
     * Initialize goalnumber, goal, gtp, and macrofix.
     * The macrofix variable is a boolean which is set if do_macro was
     * able to successfully macro expand goal into some in-line code.
     * is/2, =:=/2, and other arithmetic predicates are macro expanded for
     * integer arguments (at present).
     *------------------------------------------------------------------------*/

    goalnumber = firstgoalnumber;
    gtp = TYPEOF(goal);
    macrofix = do_macro(goal, gtp);

    for (;;) { /* Process goals: */

		/*----------------------------------------*
		 * Do compiler directives
		 *----------------------------------------*/

		compiler_directives();

		/*---------------------------------------------------------------*
		 * Get the functor id of the goal.  For example, the functor id
		 * in p(_,a,b) would be the symbol table index representing p.
		 *
		 * If a valid functor id can not be obtained, then the goal is
		 * something like a number which is invalid, so we return with
		 * an unsuccessful completion code from this function.
		 *---------------------------------------------------------------*/

		if (!(functor_id = functor_id_of_term(goal)))
		    return 0;
		garity = arity_of_term(goal);

#ifdef NewMath
		if (isarithmetic(goal)) {
		    arith_temp_homes();
		    comp_math(goal, 
					  firstgoalnumber == lastgoalarithmetic, 
					  regusemap(),
			      	  model_E - model_SP);
		    if (goalnumber == lastgoalarithmetic) {
				icode(I_INLINE_PROCEED, 0, 0, 0, 0);
				return 1;
		    }
		}	/* aritmetic */
		else 
#endif
		{	/* Non-arithmetic */
		    /*----------------------------------------*
		     * Emit the argument set up code for the goal.
		     *----------------------------------------*/

		    comp_goal(goal);

		    /*--------------------------------------------------------------*
		     * Check to see if the goal is a cutmacro.  If it is, then put
		     * something resembling the current environment pointer into the
		     * n+1st argument.  The cut point is actually the address of the
		     * last source argument.  The reason for using the last one is
		     * that this cut point may be passed determinately to other goals
		     * with successively smaller argument vectors (which will
		     * overwrite the previous arguments).
		     *--------------------------------------------------------------*/

		    if (isCutMacro(functor_id, garity)) {
				int   s = goalsize(goal);
				int   loc;

				loc = (s <= NAREGS) ? (ASTART + s - 1)
			    			: (model_Adst + goalsize(goal) - 1);
				free_target(loc);
#ifdef SlowCut
				if (functor_id == TK_CUT && garity == 0)
			    	icode(I_CUTMACRO, EREG, 0,
				  			index_of(loc), disp_of(loc));
				else
			    	icode(I_CUTMACRO, EREG, MODELSIZE - 1 - model_E,
				  			index_of(loc), disp_of(loc));
#else
				icode(I_CUTMACRO, EREG, MODELSIZE - 1 - model_E,
			      		index_of(loc), disp_of(loc));
#endif
		    } /* CutMacro */

		    /*--------------------------------------------------------------*
		     * Now we need to adjust the stack pointer back to the
		     * appropriate place for the call and then generate the call
		     * or execute.  An execute sequence will be generated for the
		     * last goal.  Call code is emitted for all other goals.
		     *--------------------------------------------------------------*/

		    if (goalnumber == ngoals) {		/* Last Goal */
				model_Eend = model_E;	/* only return address left in env */

				/*----------------------------------------*
				 * Move the return address
				 *----------------------------------------*/

#ifdef CPREG
				move(vtbl[RETIDX].home, CPREG);
#else
				move(vtbl[RETIDX].home, model_Adst - 1);
#endif
				/*----------------------------------------*
				 * Move the environment pointer
				 *----------------------------------------*/

#ifdef OLDEREG
				move(vtbl[ENVIDX].home, OLDEREG);
#else
				move(vtbl[ENVIDX].home, model_Adst - 2);
#endif

				icode(I_ADDTOSP, model_Adst - 2 - model_SP, 0, 0, 0);
				model_SP = model_Adst - 2;

				if (cd_cmod != cd_gmod)		/* change module if necessary */
		    		icode(IC_NEWMODULE, cd_gmod, 0, 0, 0);

				icode(I_EXECUTE, functor_id, garity, 0, 0);

				if (cd_cmod != cd_gmod) {	/* change it back */
				    icode(IC_ENDMODULE, 0, 0, 0, 0);
				    cd_gmod = cd_cmod;
				}

				if (macrofix)
				    icode(IC_PUTMACRO, 1, 0, 0, 0);

				return 1;	
	    		}	/* done-Last Goal */

	  	  else {	/* not-Last Goal */

			/*--------------------------------------------------------------*
			 * Adjust the stack pointer so that it points at the first
			 * destination argument.
			 *--------------------------------------------------------------*/

			icode(I_ADDTOSP, model_Adst - STACKADJUST - model_SP, 0, 0, 0);
			model_SP = model_Adst - STACKADJUST;

			/*----------------------------------------*
			 * Emit the code for doing a call.
			 *----------------------------------------*/

			if (cd_cmod != cd_gmod)
			    icode(IC_NEWMODULE, cd_gmod, 0, 0, 0);

			icode(I_CALL, functor_id, garity, 0, 0);

			if (cd_cmod != cd_gmod) {
			    icode(IC_ENDMODULE, 0, 0, 0, 0);
			    cd_gmod = cd_cmod;
			}

			/*----------------------------------------------------*
			 * Emit the garbage collector call information which
			 * must immediately follow each call.
			 *----------------------------------------------------*/

			gccallinfo();

#ifdef SlowCut
			isdeterminateforsure = (functor_id == TK_CUT && garity == 0);
#endif /* SlowCut */

		    }	/* end-not-Last Goal */
		}		/* end-non-arithmetic */

		/*----------------------------------------*
		 * Set the end of environment index.
		 *----------------------------------------*/

		model_Eend = model_E - call_env_sizes[goalnumber];

		/*---------------------------------------------------------*
		 * Put out the macro code if inline expansion code was
		 * generated for the goal.
		 *---------------------------------------------------------*/

		if (macrofix)
		    icode(IC_PUTMACRO, 0, 0, 0, 0);

		/*---------------------------------------------------------*
		 * advance the goal number and set up goal, gtp, and gsize
		 *---------------------------------------------------------*/

		goalnumber++;
		if (goalnumber == firstgoalnumber + 1)
		    reassign_homes(RULE_HEAD(rule));
		goal = RULE_GOALN(rule, goalnumber);

#ifndef SlowCut
		gtp = TYPEOF(goal);

		/*---------------------------------------------------------*
		 * Scan over all of the upcoming cuts, emit the cut code
		 * and set the isdeterminateforsure flag if a cut was seen.
		 *---------------------------------------------------------*/

		if (gtp == TP_SYM && FUNCTOR_TOKID(goal) == TK_CUT) {
		    while (goalnumber < ngoals && gtp == TP_SYM &&
			   FUNCTOR_TOKID(goal) == TK_CUT) {
			goalnumber++;
			goal = RULE_GOALN(rule, goalnumber);
			gtp = TYPEOF(goal);
		    }

		    if (goalnumber == ngoals && gtp == TP_SYM &&
				FUNCTOR_TOKID(goal) == TK_CUT) {
				icode(I_DEALLOCATE_CUT_PROCEED, 0, 0, 0, 0);
				return 1;
		    }
		    icode(I_DOCUT, EREG, 0, 0, 0);
		    isdeterminateforsure = 1;
		    model_Eend = model_E - call_env_sizes[goalnumber - 1];
		}
		else
			isdeterminateforsure = 0;
#endif /* SlowCut */

		/*---------------------------------------------------------*
		 * See if there are any compiler directives on this goal
		 *---------------------------------------------------------*/

		goal = get_compiler_directives(goal);
		gtp = TYPEOF(goal);
		gsize = goalsize(goal);

		/*--------------------------------------------------------------*
		 * Generate and save inline code in a secret place if the next
		 * goal can be macro expanded.  The reason we generate it here
		 * is so that we can skip over the trim or deallocate code if
		 * at all possible.
		 *--------------------------------------------------------------*/

		macrofix = do_macro(goal, gtp);

		/*--------------------------------------------------------------*
		 * Put out the trim or deallocate code.  Also set up model_SP,
		 * model_Adst, model_Adst2, and the targets for the next goal.
		 *--------------------------------------------------------------*/

		if (goalnumber < ngoals) {

#ifdef NewMath
		    int   afix;

		    /*------------------------------------------------------------*
			 * When compiling a math expression, we want to use the size
		     * of the environment in the previous goal in order to avoid
		     * overwriting a crucial environment variable during a callout.
		     * The afix flag tells us if we saw an arithmetic goal.
		     *------------------------------------------------------------*/
		    if ( (afix = isarithmetic(goal)) )	/* want single = here */
				goalnumber--;
#endif
		    /*-----------------------------------------------------------*
		     * The second argument in the following call gives the size of
		     * the current environment.
		     *
		     * The third argument gives the value to add on to the second
		     * argument for setting the stack pointer.  The reason for the
		     * maximum function is to prevent us from setting the stack
		     * pointer in the middle of the environment variables to be
		     * trimmed away. (They still may be used to set up arguments 
			 * in the next goal.)
		     *-----------------------------------------------------------*/

		    icode(I_TRIM,
			  call_env_sizes[goalnumber],
		    max(call_env_sizes[goalnumber - 1] - call_env_sizes[goalnumber],
				gsize) + STACKADJUST,
				  isdeterminateforsure, 0);
		    model_Adst = model_E - call_env_sizes[goalnumber] - gsize;

#ifdef NewMath
		    if (afix)
				goalnumber++;
#endif

		    /*----------------------------------------------------------*
		     * Make stack pointer agree with the trim call above.
		     *----------------------------------------------------------*/

		    if (model_Adst < model_Eend)
				model_SP = model_Adst - STACKADJUST;
		    else
				model_SP = model_Eend - STACKADJUST;

		    /*----------------------------------------------------------*
		     * Make model_Adst2 zero since we will not be overwriting
		     * the original arguments.
		     *----------------------------------------------------------*/

		    model_Adst2 = 0;

		    init_targets(goal);
		}
		else
		    deallocate_environment(goal, gtp, gsize, isdeterminateforsure);

		/*------------------------------------------------------------*
		 * model_SPstart is the place where the stack pointer starts
		 * out at prior to the compilation of the goal.  Its only
		 * purpose is so that we know where to start reallocating
		 * temporaries off of the stack.
		 *------------------------------------------------------------*/

		model_SPstart = model_SP;

    } 	/* end-Process goals: */
}		/* comp_rule */

/*========================================================================*
 * deallocate_environment is called for deallocating an environment just
 * prior to setting up the arguments for the last goal.
 *========================================================================*/

static void
deallocate_environment(pword goal, int gtp, int gsize, int isdeterminateforsure)
{
    int   esize = call_env_sizes[goalnumber - 1];
    int   s, i, t, p, v;
    pword ag;

    icode(I_DEALLOCATE1, 0, 0, 0, 0);

    /*-----------------------------------------------------------------*
     * Set model_Adst, model_Adst2, and a guess at model_SP.
     *-----------------------------------------------------------------*/

    model_Adst = model_E - gsize;
    model_Adst2 = MODELSIZE - gsize;
    model_SP = LASTREG + 1;	/* This will not be right. */

    /*
     * Initialize targets for the last goal.
     */

    init_targets(goal);

    /*-----------------------------------------------------------------*
     * Scan the goal arguments.  When a goal argument is the same variable
     * as a still living head argument, check on the corresponding position
     * for the non-determinate case.  If this position is still a variable
     * move it if it is safe.  (If it is unsafe, it causes lots of headaches
     * to think about moving it, though in theory it is possible).
     *
     * We can not rely on the stack pointer to stay in the same place,
     * so any temporaries which are allocated must be machine registers.
     * (That is to say, DON'T allocate temporaries off of the stack.)
     *-----------------------------------------------------------------*/

    if (gtp == TP_TERM) {
	i = TERM_ARITY(goal);
	s = MODELSIZE - 1 - (gsize - i);
	for (; i > NAREGS && s > model_E + 1; i--, s--) {
	    ag = TERM_ARGN(goal, i);
	    if (TYPEOF(model[s]) == TP_VO && TYPEOF(ag) == TP_VO &&
		(VO_VAL(model[s])) == VO_VAL(ag) &&
		TYPEOF(model[p = model_Adst + i - 1]) == TP_VO &&
		!vtbl[v = VO_VAL(model[p])].unsafe &&
		vtbl[v].pvnum &&
		vtbl[v].home == p) {
		if (!(t = find_temp_in_reg(vtbl[v].target)))
		    break;
		move(p, t);
		model[t] = model[p];
		vtbl[v].home = t;
		vtbl[v].pvnum = 0;
		model[p] = NIL_VAL;
	    }
	}
    }

    /*-----------------------------------------------------------------*
     * Do a similar check for the return address and Old Environment ptr
     *-----------------------------------------------------------------*/

    if (model_Adst2 - 2 == model_E) {

#ifndef CPREG
	if (TYPEOF(model[model_Adst - 1]) == TP_VO &&
	    !vtbl[v = VO_VAL(model[model_Adst - 1])].unsafe &&
	    vtbl[v].home == model_Adst - 1) {
	    t = find_temp_in_reg(vtbl[v].target);
	    if ( t ) {
		move(model_Adst - 1, t);
		model[t] = model[model_Adst - 1];
		vtbl[v].home = t;
		vtbl[v].pvnum = 0;
		model[model_Adst - 1] = NIL_VAL;
	    }
	}
#endif

#ifndef OLDEREG
	if (TYPEOF(model[model_Adst - 2]) == TP_VO &&
	    !vtbl[v = VO_VAL(model[model_Adst - 2])].unsafe &&
	    vtbl[v].home == model_Adst - 2) {
	    t = find_temp_in_reg(vtbl[v].target);
	    if (t) {
		move(model_Adst - 2, t);
		model[t] = model[model_Adst - 2];
		vtbl[v].home = t;
		vtbl[v].pvnum = 0;
		model[model_Adst - 2] = NIL_VAL;
	    }
	}
#endif
    }

    /*-----------------------------------------------------------------*
     * The icode call to I_DEALLOCATE2 will test to see if we are determinate
     * or not.  If we are non-determinate, it will set the stack pointer
     * appropriately.  The nasty max computation is the number of longwords
     * to subtract from SPB to set SP correctly.
     *-----------------------------------------------------------------*/

    if (!isdeterminateforsure)
	icode(I_DEALLOCATE2, max(esize + MODELSIZE - model_E, gsize + 2), 0, 0, 0);

    /*
     * Set model_SP to reflect the above call (or the call which will happen
     * for the determinate case below).
     */

    model_SP = model_E - max(esize + MODELSIZE - model_E, gsize + 2);

    /*
     * Move variables occupying the same positions in the determinate
     * case.
     */

    if (gtp == TP_TERM) {
	i = TERM_ARITY(goal);
	s = MODELSIZE - 1 - (gsize - i);
	for (; i > NAREGS && s > model_E + 1; i--, s--) {
	    ag = TERM_ARGN(goal, i);
	    if (TYPEOF(model[s]) == TP_VO &&
		TYPEOF(ag) == TP_VO &&
		(v = VO_VAL(model[s])) == VO_VAL(ag) &&
		model[model_Adst + i - 1] == NIL_VAL) {
		if (!isdeterminateforsure)
		    move(s, model_Adst + i - 1);
		model[model_Adst + i - 1] = model[s];
		vtbl[v].home = model_Adst + i - 1;
	    }
	}
    }

    /*-----------------------------------------------------------------*
     * Move return address and Old E if they live in the same place
     *-----------------------------------------------------------------*/

    if (model_Adst2 - 2 == model_E) {
#ifndef CPREG
	if (model[model_Adst - 1] == NIL_VAL) {
	    if (!isdeterminateforsure)
		move(model_E + 1, model_Adst - 1);
	    model[model_Adst - 1] = model[model_E + 1];
	    vtbl[RETIDX].home = model_Adst - 1;
	}
#endif
#ifndef OLDEREG
	if (model[model_Adst - 2] == NIL_VAL) {
	    if (!isdeterminateforsure)
		move(model_E, model_Adst - 2);
	    model[model_Adst - 2] = model[model_E];
	    vtbl[ENVIDX].home = model_Adst - 2;
	}
#endif
    }

    /*-----------------------------------------------------------------*
     * Put down the branch around the code for setting up the determinate
     * case
     *-----------------------------------------------------------------*/

    if (!isdeterminateforsure)
	icode(I_DEALLOCATE3, 0, 0, 0, 0);
    /*-----------------------------------------------------------------*
     * Generate code for setting the determinate case.
     *-----------------------------------------------------------------*/

    icode(I_DEALLOCATE4, max(esize, gsize + 2 - (MODELSIZE - model_E)), 0, 0, 0);
}

/*============================================================================*
 * gccallinfo puts out the instruction just following a jsr which the garbage
 * compacter uses to determine the environment size and argument usage.
 *
 *============================================================================*/

void
gccallinfo(void)
{
    register int i;
    register int v;
    register int limit;
    long  mask = 0;


    /*---------------------------------------------------------------------*
     * Set limit to the place to scan to in the model.  We are limited to
     * setting usage bits for 32 of the arguments.
     *---------------------------------------------------------------------*/

    limit = model_E + 34;
    if (limit > MODELSIZE)
	limit = MODELSIZE;

    /*---------------------------------------------------------------------*
     * Create a usage mask.  The usage mask bits are set corresponding to
     * arguments which are still being used.  The least significant bit
     * corresponds to the first argument.
     *---------------------------------------------------------------------*/

    for (i = model_E + 2; i < limit; i++) {
	if (IS_VO(model[i])) {
	    v = VO_VAL(model[i]);
	    if (vtbl[v].usecnt < vtbl[v].noccurences)
		mask |= 1L << (i - (model_E + 2));
	}
    }

    /*---------------------------------------------------------------------*
     * Put out the mask, the number of arguments and the number of
     * permanent variables.
     *
     * (The number of arguments is given by MODELSIZE-model_E-2 since
     * gccallinfo is being called after the model has been informed
     * that the return address and Old E have been put down.)
     *---------------------------------------------------------------------*/

    /*---------------------------------------------------------------------*
     * icode(I_CALLINFO,mask,MODELSIZE-model_E-2,model_E-model_Eend,0);
     *---------------------------------------------------------------------*/
    icode(I_CALLINFO, mask, MODELSIZE - model_E - 2, call_env_sizes[goalnumber], 0);
}

/*============================================================================*
 * The following function is called to initialize environment variables to
 * unbound just prior to the first goal in a multi-goal clause.
 *============================================================================*/

static void
initialize_environment_variables(void)
{
    register int i;
    register int v;
    register int inc;

    for (i = model_Eend; i < model_E; i++) {
	if (IS_VO(model[i])) {
	    v = VO_VAL(model[i]);
	    if (vtbl[v].firstocc > firstgoalnumber) {
		icode(I_INIT_YVAR1, EREG, i - model_E, 0, 0);

		/*---------------------------------------------------------------------*
		 * The use count is not advanced in order that the code
		 * which eventually use it will reinitialize it.  This
		 * is probably better since if the variable occurs
		 * in structure a unify_local_value will not have to be done.
		 *
		 * Otherwise, the following two lines would appear here. These
		 * would prevent code from re-initializing environment
		 * variables:
		 *
		 *      vtbl[v].usecnt++;
		 *      vtbl[v].noccurences++;
		 *---------------------------------------------------------------------*/

		break;
	    }
	}
    }

    for (i++, inc = 0; i < model_E; i++, inc++) {
	if (IS_VO(model[i])) {
	    v = VO_VAL(model[i]);
	    if (vtbl[v].firstocc > firstgoalnumber) {
		icode(I_INIT_YVAR2, inc, 0, 0, 0);
		inc = -1;
	    }
	}
    }
}

#if defined(InMath) && !defined(NewMath)
static long
regfreemap(void)
{
    register int i;
    register int map = 0;

#if NTREGS > 0
    for (i = TSTART; i <= TEND; i++)
	if (model[i] == NIL_VAL)
	    map |= (1 << i);
#endif
#if NAREGS > 0
    for (i = ASTART; i <= AEND; i++)
	if (model[i] == NIL_VAL)
	    map |= (1 << i);
#endif
    return map;
}
#endif /* InMath && !NewMath */

#ifdef NewMath

static long
regusemap(void)
{
    register int i;
    register int map = 0;

#if NTREGS > 0
    for (i = TSTART; i <= TEND; i++)
	if (model[i] != NIL_VAL)
	    map |= (1 << i);
#endif
#if NAREGS > 0
    for (i = ASTART; i <= AEND; i++)
	if (model[i] != NIL_VAL)
	    map |= (1 << i);
#endif
    return map;
}

/*============================================================================*
 * isarithmetic takes a goal and determines if it is one of the ones for
 * which we should compile inline arithmetic for.
 *
 *============================================================================*/

int
isarithmetic(pword goal)
{
    int   f;

    if (TYPEOF(goal) == TP_TERM && TERM_ARITY(goal) == 2 &&
	(f = FUNCTOR_TOKID(TERM_FUNCTOR(goal))) >= TK_IS &&
	f <= TK_ZEBRA2)
	return 1;
    else
	return 0;
}

/*============================================================================*
 * arith_temp_homes allocates space for any temporaries, should
 * they exist.  This is a perverse situation, but must be handled.
 *
 * This procedure will only affect variables whose first and last occurrences
 * are in the arithmetic expression itself, as in
 *
 *      p(Y) :- Y is X*X.
 *
 * The only time in which this could conceivably be useful would be in
 * something of the form:
 *
 *      p(Y) :- Y is g1(X)*g2(X).
 *
 * where g1 and g2 have extended is/2 and X is used to pass information between
 * the two.  Even then, I doubt that it has much usefulness.
 *============================================================================*/

static void
arith_temp_homes(void)
{
    register int v;
    int   n;

    for (v = 0, n = 0; v < nvars; v++) {
	if (vtbl[v].firstocc == goalnumber &&
	    vtbl[v].lastocc == goalnumber &&
	    vtbl[v].noccurences > 1) {
	    n--;
	    vtbl[v].home = model_Eend + n;
	}
    }
    icode(I_ADDTOSP, n, 0, 0, 0);
    model_SP += n;
    model_Eend += n;
}
#endif /* NewMath */

static int
do_macro(pword goal, int gtp)
{
    int   macrofix = 0;

#ifndef	NewMath
#ifdef	InMath
    if (gtp == TP_TERM && TERM_ARITY(goal) == 2) {
	register int tokid;

	tokid = FUNCTOR_TOKID(TERM_FUNCTOR(goal));
	if (TK_IS <= tokid && tokid <= TK_ZEBRA2) {

	    icode(IC_BEGINMACRO, 0, 0, 0, 0);	/* starts macro capture mode */
	    icode(I_MTH_INIT, regfreemap(), 0, 0, 0);
	    macrofix = comp_math(goal, goalnumber == ngoals,
				 goalnumber > firstgoalnumber);
	    icode(IC_ENDMACRO, macrofix, 0, 0, 0);	/* ends macro capture
							 * mode
							 */
	}
    }

#endif /* InMath */
#endif /* NewMath */

    return macrofix;
}

/*============================================================================*
 * goalsize is given a structure purported to be a goal.  It returns the
 *      number of arguments of the goal.  If the goal is a cutmacro, it
 *      returns the number of arguments plus 1.
 *============================================================================*/

static int
goalsize(pword goal)
{
    int   size;
    int   func;
    int   gtp;

    gtp = TYPEOF(goal);
    if (gtp == TP_TERM) {
	size = TERM_ARITY(goal);
	func = FUNCTOR_TOKID(TERM_FUNCTOR(goal));
    }
    else if (gtp == TP_SYM) {
	size = 0;
	func = FUNCTOR_TOKID(goal);
    }
    else {
	size = 0;
	func = TK_NIL;
    }

    if (isCutMacro(func, size))
	size++;

    return size;
}

static void
incr_usecnt(int v)
{
    if (++vtbl[v].usecnt == vtbl[v].noccurences) {
	model[vtbl[v].home] = NIL_VAL;
    }
}

/*============================================================================*
 * init_model is responsible for initializing the model of the machine
 *      prior to the start of the compilation.
 *============================================================================*/

static void
init_model(pword head, pword firstgoal)
{
    register int i;
    int   firstgoalsize = goalsize(firstgoal);
    pword s;

    model_E = MODELSIZE - 2;	/* room for return address and prev env */
    nargs = 0;
    nargsmatched = 0;		/* haven't matched any arguments yet. */

    /*---------------------------------------------------------------------*
     * Install the head arguments into the appropriate part of the model.
     *---------------------------------------------------------------------*/

    if (TYPEOF(head) == TP_TERM) {
	register int ti, v;

	nargs = TERM_ARITY(head);
	model_E -= nargs;

	/*---------------------------------------------------------------------*
	 * Take care of the register part of the model first.
	 *---------------------------------------------------------------------*/

	for (i = 1; i <= NAREGS && i <= nargs; i++) {
	    s = TERM_ARGN(head, i);
	    ti = ASTART + i - 1;
	    if (IS_VO(s) && vtbl[v = VO_VAL(s)].pvnum == 0 &&
		vtbl[v].home == 0) {
		vtbl[VO_VAL(s)].home = ti;
		model[ti] = s;
		model[model_E + i + 1] = s;
		if (vtbl[v].lastocc > firstgoalnumber)
		    vtbl[v].target = model_E + i + 1;	/* this may be
							 * reassigned
							 */
		incr_usecnt(VO_VAL(s));
	    }
	    else {
		model[ti] = s;
		model[model_E + i + 1] = NIL_VAL;
	    }
	}

	/*
	 * Now take care of the overflow case.
	 */

	for (i = NAREGS + 1; i <= nargs; i++) {
	    s = TERM_ARGN(head, i);
	    ti = model_E + i + 1;

	    if (IS_VO(s) && vtbl[VO_VAL(s)].pvnum == 0 &&
		vtbl[VO_VAL(s)].home == 0) {
		vtbl[VO_VAL(s)].home = ti;
		model[ti] = s;
		incr_usecnt(VO_VAL(s));
	    }
	    else
		model[ti] = s;
	}
    }

    /*---------------------------------------------------------------------*
     * Initialize those registers not covered by the arguments (if any).
     *---------------------------------------------------------------------*/

    for (i = nargs + 1; i <= NAREGS; i++) {
	model[ASTART + i - 1] = NIL_VAL;
    }

    model_Eend = model_E;	/* don't want to clobber return address */
    model_SP = model_Eend;

    for (i = 0; i < nvars; i++) {
	if (vtbl[i].pvnum) {
	    vtbl[i].home = model_E - vtbl[i].pvnum;
	    model[vtbl[i].home] = MK_VO(i);
	}
    }

    /*---------------------------------------------------------------------*
     * Set the previous environment value up as a variable so that it can
     * be moved.
     *---------------------------------------------------------------------*/

#ifdef OLDEREG
    vtbl[ENVIDX].home = OLDEREG;
    model[model_E] = NIL_VAL;
    model[OLDEREG] = MK_VO(ENVIDX);
#else
    vtbl[ENVIDX].home = model_E;
    model[model_E] = MK_VO(ENVIDX);
#endif
    vtbl[ENVIDX].firstocc = 0;
    vtbl[ENVIDX].istoplevinhead = 0;
    vtbl[ENVIDX].lastocc = ngoals;
    vtbl[ENVIDX].noccurences = 2;
    vtbl[ENVIDX].pvnum = 0;
    vtbl[ENVIDX].usecnt = 1;
    vtbl[ENVIDX].target = 0;
    vtbl[ENVIDX].unsafe = 0;

    /*---------------------------------------------------------------------*
     * Set the return pointer up as a variable so that it can be moved
     *---------------------------------------------------------------------*/

#ifdef CPREG
    vtbl[RETIDX].home = CPREG;
    model[model_E + 1] = NIL_VAL;
    model[CPREG] = MK_VO(RETIDX);
#else
    vtbl[RETIDX].home = model_E + 1;
    model[model_E + 1] = MK_VO(RETIDX);
#endif
    vtbl[RETIDX].firstocc = 0;
    vtbl[RETIDX].istoplevinhead = 0;
    vtbl[RETIDX].lastocc = ngoals;
    vtbl[RETIDX].noccurences = 2;
    vtbl[RETIDX].pvnum = 0;
    vtbl[RETIDX].usecnt = 1;
    vtbl[RETIDX].target = 0;	/* no target yet */
    vtbl[RETIDX].unsafe = 0;	/* bad idea to emit a put_unsafe */

    icode(IC_BEGINALLOC, 0, 0, 0, 0);	/* indicate to code generator that
					 * we are allocating environment/
					 * stack args for first goal
					 */
    if (ngoals - firstgoalnumber > 0) {
	icode(I_ALLOCATE,
	      npv + firstgoalsize + STACKADJUST,
	      0, 0, 0);
	model_Eend = model_E - npv;
	model_SP = model_Eend - firstgoalsize - STACKADJUST;
	model_Adst = model_Eend - firstgoalsize;
	model_Adst2 = 0;
	init_targets(firstgoal);
    }
    else if (ngoals - firstgoalnumber == 0) {
	model_Eend = model_E;
	model_Adst = model_E - firstgoalsize;
	model_Adst2 = MODELSIZE - firstgoalsize;
	model[model_Adst - 1] = NIL_VAL;	/* this is where we'll put the
						 * return address
						 */

	if (firstgoalnumber == 1) {
	    init_targets(firstgoal);
	    icode(I_ALLOCATE1, nargs + 2, 0, 0, 0);
	    model_SP -= (nargs + 2);

	    /*---------------------------------------------------------------------*
	     * The stuff in the following if statement will move variables
	     * living in the same corresponding locations for the non-determinate
	     * case.  The reason for the comparison of the firstgoalsize with
	     * the arity is to avoid running this code when the first goal is
	     * a cut macro (which has a goal size one greater than the arity)
	     *---------------------------------------------------------------------*/

	    if (TYPEOF(head) == TYPEOF(firstgoal)) {
		if (TYPEOF(head) == TP_TERM &&
		    firstgoalsize == TERM_ARITY(firstgoal)) {
		    int   j;
		    pword ah, ag;

		    for (i = TERM_ARITY(head), j = TERM_ARITY(firstgoal);
			 i > NAREGS && j > NAREGS;
			 i--, j--) {
			ah = TERM_ARGN(head, i);
			ag = TERM_ARGN(firstgoal, j);
			if (TYPEOF(ah) == TP_VO && TYPEOF(ag) == TP_VO &&
			    VO_VAL(ah) == VO_VAL(ag)) {
			    move(model_E + i + 1, model_SP + i + 1);
			    model[model_SP + i + 1] = model[model_E + i + 1];
			}
		    }
		    if (i == j) {
#ifndef CPREG
			move(model_E + 1, model_SP + 1);	/* move
								 * return
								 * address
								 */
			model[model_SP + 1] = model[model_E + 1];
			vtbl[RETIDX].home = model_SP + 1;
#endif

#ifndef OLDEREG
			move(model_E, model_SP);	/* move env pointer */
			model[model_SP] = model[model_E];
			vtbl[ENVIDX].home = model_SP;
#endif
		    }
		}
		else if (firstgoalsize == 0) {
#ifndef CPREG
		    move(model_E + 1, model_SP + 1);	/* move return
							 * address
							 */
		    model[model_SP + 1] = model[model_E + 1];
		    vtbl[RETIDX].home = model_SP + 1;
#endif

#ifndef OLDEREG
		    move(model_E, model_SP);	/* move env pointer */
		    model[model_SP] = model[model_E];
		    vtbl[ENVIDX].home = model_SP;
#endif
		}
	    }

	    icode(I_ENDALLOC1, 0, 0, 0, 0);
	    icode(I_ADDTOSP, -max(0, firstgoalsize - nargs), 0, 0, 0);
	    vtbl[RETIDX].target = model_Adst - 1;
	    vtbl[ENVIDX].target = model_Adst - 2;
	    model_SP -= max(0, firstgoalsize - nargs);
	    for (i = model_SP; i < model_Adst - 2; i++)
		model[i] = NIL_VAL;
	}
	else {
	    /*---------------------------------------------------------------------*
	     * only goal will be determinate due to a cut.  In this case, we
	     * may avoid doing the allocate1 code since it is pointless to
	     * set things up for the non-determinate case.  But we don't want
	     * the compiler to overwrite things in case it isn't so we don't
	     * let any of the source locations be targeted.
	     *---------------------------------------------------------------------*/

	    model_Eend = model_E;
	    model_Adst = MODELSIZE - firstgoalsize;
	    model_Adst2 = model_Adst;	/* this will get us the variable
					 * movement optimizations
					 */
	    init_targets(NIL_VAL);	/* still need to do some of the
					 * initializations
					 */
	    icode(I_ADDTOSP, -max(0, firstgoalsize - nargs), 0, 0, 0);
	    vtbl[RETIDX].target = model_Adst - 1;
	    model_SP -= max(0, firstgoalsize - nargs);
	    for (i = model_SP; i < model_Eend - 1; i++)
		model[i] = NIL_VAL;
	}

    }
    else {			/* no goals */
	model_Adst = model_Eend;
	model_Adst2 = 0;
	init_targets(firstgoal);
    }
    icode(IC_ENDALLOC, 0, 0, 0, 0);
    model_SPstart = model_SP;
}

/*============================================================================*
 * init_targets scans the given goal and looks for top-level temporary
 *      variables.  It initializes the target field in the vtbl array
 *      for the variable to be the location that this variable should be
 *      moved to.  When we attempt to move this variable later on, or we
 *      want to find a home for it, a good attempt will be made to make
 *      the variable live in the target location.
 *
 * init_targets is usually called just prior to compilation of a goal.  Thus
 *      it performs other initializations which are important.  The portion
 *      of the model corresponding to the machine registers is NIL'd out thus
 *      freeing them up for allocation.  This is not dangerous due to our
 *      assumption that these machine registers do not survive between goals.
 *      (They do however survive between the head and the first goal).  Those
 *      portions of the target locations for the goal which do not overlap
 *      part of the existing environment are also NIL'd out.  These locations
 *      correspond to portions of the stack yet to be allocated and so are
 *      assumed to be free.
 *============================================================================*/

static void
init_targets(pword t)
{
    register int i;

    /*---------------------------------------------------------------------*
     * Initialize that portion of the model corresponding to the machine
     * temporary registers
     *---------------------------------------------------------------------*/

    for (i = TSTART; i <= TEND; i++)
	model[i] = NIL_VAL;

    if (goalnumber > firstgoalnumber) {
	for (i = ASTART; i <= AEND; i++)
	    model[i] = NIL_VAL;
    }

    /*---------------------------------------------------------------------*
     * Initialize the destination argument positions that do not overlap
     * the environment.
     *---------------------------------------------------------------------*/

    for (i = model_Adst - 2; i < model_Eend; i++)
	model[i] = NIL_VAL;

    /*---------------------------------------------------------------------*
     * Initialize the target fields in the vtbl array.
     *---------------------------------------------------------------------*/

    init_only_targets(t);

    if (goalnumber == ngoals) {
#ifdef CPREG
	vtbl[RETIDX].target = CPREG;
#else
	vtbl[RETIDX].target = model_Adst - 1;
#endif

#ifdef OLDEREG
	vtbl[ENVIDX].target = OLDEREG;
#else
	vtbl[ENVIDX].target = model_Adst - 2;
#endif
    }
}

/*============================================================================*
 * init_only_targets is called by init_targets to initialize the target
 *      fields in the vtbl array.  No other initializations are performed.
 *      This function is also called just prior to compilation of G in
 *      something of the form P :- !, G.  In this case it is desirable to
 *      initialize the target fields, but not perform the other initializations
 *      found in init_targets.
 *============================================================================*/

static void
init_only_targets(pword t)
{
    pword s;
    register int i, n;


    if (goalnumber >= firstgoalnumber)
	for (n = nvars, i = 0; i < n; i++)
	    vtbl[i].target = 0;	/* clear the targets */

    if (TYPEOF(t) == TP_TERM) {
	n = TERM_ARITY(t);
	for (i = 1; i <= n; i++) {
	    s = TERM_ARGN(t, i);
	    if (IS_VO(s) && vtbl[VO_VAL(s)].target == 0) {
		vtbl[VO_VAL(s)].target =
		    ((i <= NAREGS) ? ASTART : model_Adst) + i - 1;
	    }
	}
    }
}

/*============================================================================*
 * reassign_homes is called just after the first goal to set up homes for those
 * variables which lived in registers during the compilation of the head and
 * first goal.
 *
 * Note that the for loop starts at head_nargs and counts down to 1.  The
 * reason for counting down instead of up is to assign the home field to the
 * lowest model location in the case of duplicate variables.  The loop
 * was originally coded to count up instead of down and lead to a strange
 * bug on the SPARC manifested by the following clause:
 *      foo(X,X) :- nop, pbi_write(X), pbi_nl.
 *============================================================================*/

static void
reassign_homes(pword head)
{
    if (TYPEOF(head) == TP_TERM) {
	register int i;
	register pword s;
	int   head_nargs, ti, v;

	head_nargs = TERM_ARITY(head);
	if (head_nargs > NAREGS)
	    head_nargs = NAREGS;

	for (i = head_nargs; i >= 1; i--) {
	    s = TERM_ARGN(head, i);
	    ti = model_E + i + 1;

	    if (IS_VO(s) && vtbl[v = VO_VAL(s)].pvnum == 0 &&
		vtbl[v].usecnt < vtbl[v].noccurences) {
		vtbl[v].home = ti;
		vtbl[v].target = 0;
		model[ti] = s;
	    }
	}
    }

#ifdef OLDEREG
    vtbl[ENVIDX].home = model_E;
    model[model_E] = MK_VO(ENVIDX);
#endif

#ifdef CPREG
    vtbl[RETIDX].home = model_E + 1;
    model[model_E + 1] = MK_VO(RETIDX);
#endif
}

static void
comp_head(pword head)
{
    int   i;
    pword arg;
    int   tp;
    int   ti;

    if (TYPEOF(head) != TP_TERM) {
	record_first_argument(TP_VO, (pword) 0);
	return;
    }

    for (i = 1; i <= TERM_ARITY(head); i++) {
	nargsmatched = i;
	if (i <= NAREGS) {
	    ti = ASTART + i - 1;
	}
	else {
	    ti = model_E + i + 1;
	}
	arg = TERM_ARGN(head, i);
	tp = TYPEOF(arg);
	switch (tp) {
	    case TP_SYM:
		icode(I_G_SYM, FUNCTOR_TOKID(arg), index_of(ti), disp_of(ti), 0);
		model[ti] = NIL_VAL;
		break;
	    case TP_INT:
		icode(I_G_INT, INT_VAL(arg), index_of(ti), disp_of(ti), 0);
		model[ti] = NIL_VAL;
		break;
	    case TP_VO:{
		    int   v = VO_VAL(arg);

		    if (TYPEOF(model[ti]) == TP_VO &&
			VO_VAL(model[ti]) == v) {
			if (vtbl[v].usecnt == 0) {
			    move(ti, vtbl[v].home);
			    vtbl[v].usecnt++;
			}
			else if (vtbl[v].home != ti) {
			    icode(I_G_VALUE, index_of(ti), disp_of(ti),
			     index_of(vtbl[v].home), disp_of(vtbl[v].home));
			    incr_usecnt(v);
			}
			if (vtbl[v].usecnt == vtbl[v].noccurences)
			    model[ti] = NIL_VAL;	/* only nil it out
							 * when  we are
							 * through with it
							 */
		    }
		}
		break;

	    case TP_UIA:
	    case TP_TERM:
	    case TP_LIST:
#ifdef DoubleType
	    case TP_DOUBLE:
#endif
		comp_head_structure(ti);
		break;
	    default:
		printf("Something funny in comp_head!\n");
		break;
	}
	if (i == 1)
	    record_first_argument(tp, arg);
    }
}

/*============================================================================*
 * record_first_argument is used to aid first argument indexing.
 *============================================================================*/

static void
record_first_argument(int tp, pword arg)
{
    switch (tp) {
	case TP_SYM:
	    icode(IC_1STARG, TP_SYM, FUNCTOR_TOKID(arg), 0, 0);
	    break;
	case TP_INT:
	    icode(IC_1STARG, TP_INT, TK_NIL, 0, INT_VAL(arg));
	    break;
	case TP_UIA:
	case TP_VO:
	    icode(IC_1STARG, TP_VO, TK_NIL, 0, 0);
	    break;
	case TP_LIST:
	    icode(IC_1STARG, TP_LIST, TK_NIL, 0, 0);
	    break;
	case TP_TERM:
	    icode(IC_1STARG, TP_SYM,
		  FUNCTOR_TOKID(TERM_FUNCTOR(arg)),
		  TERM_ARITY(arg),
		  0);
	    break;
    }
}

/*============================================================================*
 * comp_head_structure(loc)
 *
 *      This function compiles lists and structures occuring in the head.
 *      loc is an index into the model at which we will find the structure
 *      to match against
 *============================================================================*/

static void
comp_head_structure(int loc)
{
    int   tdbase;
    pword strct;
    int   i, tp;

    strct = model[loc];
    tp = TYPEOF(strct);

    if (tp == TP_UIA) {
	icode(I_G_UIA, (long) UIA_NAME(strct), index_of(loc), disp_of(loc), 0);
	model[loc] = NIL_VAL;
    }
#ifdef DoubleType
    else if (tp == TP_DOUBLE) {
	icode(I_G_DBL, DOUBLE_VAL1(strct), DOUBLE_VAL2(strct),
	      index_of(loc), disp_of(loc));
	model[loc] = NIL_VAL;
    }
#endif
    else {			/* structure or list */

	TDT_SET(tdbase, 0);

	icode(I_START_CAPTURE, 0, 0, 0, 0);

	if (tp == TP_LIST) {
	    icode(I_G_LIST, index_of(loc), disp_of(loc), 0, 0);
	    model[loc] = NIL_VAL;
	    comp_hstruct_argument(LIST_CAR(strct), 0);
	    comp_hstruct_argument(LIST_CDR(strct), 1);
	}
	else {			/* must be a structure */
	    icode(I_G_STRUCTURE, functor_id_of_term(strct), arity_of_term(strct),
		  index_of(loc), disp_of(loc));
	    model[loc] = NIL_VAL;
	    for (i = 1; i <= TERM_ARITY(strct); i++)
		comp_hstruct_argument(TERM_ARGN(strct, i), i - 1);
	}

	icode(I_END_CAPTURE, 0, 0, 0, 0);

	for (i = tdbase; i < to_do_top; i++)
	    comp_head_structure(to_do[i]);
	TDT_RESET(tdbase);
    }
}

/*============================================================================*
 * comp_hstruct_argument(arg,n)
 *
 *      This function compiles the nth argument which occurs in a list or
 *      structure appearing in the head.
 *============================================================================*/

static void
comp_hstruct_argument(pword arg, int n)
{
    int   t, v;

    switch (TYPEOF(arg)) {
	case TP_INT:
	    icode(I_U_INT, INT_VAL(arg), n, 0, 0);
	    break;
	case TP_SYM:
	    icode(I_U_SYM, FUNCTOR_TOKID(arg), n, 0, 0);
	    break;
	case TP_UIA:
	case TP_LIST:
	case TP_TERM:
#ifdef DoubleType
	case TP_DOUBLE:
#endif /* DoubleType */
	    t = find_temp();
	    model[t] = arg;
	    to_do[to_do_top++] = t;
	    icode(I_U_VAR, index_of(t), disp_of(t), n, 0);
	    break;
	case TP_VO:
	    v = VO_VAL(arg);
	    if (vtbl[v].noccurences == 1) {
		icode(I_U_VOID, n, 0, 0, 0);
		vtbl[v].usecnt++;
	    }
	    else {
		t = find_home(v);
		if (vtbl[v].usecnt == 0) {
		    icode(I_U_VAR, index_of(t), disp_of(t), n, 0);
		    vtbl[v].unsafe = 0;
		}
		else if (vtbl[v].unsafe)
		    icode(I_U_LVAL, index_of(t), disp_of(t), n, 0);
		else
		    icode(I_U_VAL, index_of(t), disp_of(t), n, 0);
		vtbl[v].usecnt++;
		if (vtbl[v].usecnt == vtbl[v].noccurences)
		    model[t] = NIL_VAL;
	    }
	    break;
	default:
	    printf("Something funny in comp_hstruct_argument\n");
	    break;
    }
}

/*============================================================================*
 * index_of(n)
 *      takes a position in the model and determines which register to index
 *      off of.
 *============================================================================*/

int
index_of(int n)
{
    if (n <= LASTREG)
	return REGS;
    else if (n >= model_E)
	return EREG;
    else if (n >= model_Eend && TYPEOF(model[n]) == TP_VO &&
	     vtbl[VO_VAL(model[n])].pvnum)
	return EREG;
    else
	return SPREG;
}

/*============================================================================*
 * disp_of(n)
 *      takes a position in the model and return the displacement from the
 *      register to be indexed off of (determined by index_of)
 *============================================================================*/

int
disp_of(int n)
{
    int   idx = index_of(n);

    if (idx == REGS)
	return n;
    else if (idx == EREG)
	return n - model_E;
    else
	return n - model_SP;
}

static void
move(int n, int m)
{
    if (n == m && TYPEOF(model[m]) == TP_VO
	&& vtbl[VO_VAL(model[m])].pvnum == 0)
	return;
    /* don't move it if it's already in place */
    icode(I_MOVE, index_of(n), disp_of(n), index_of(m), disp_of(m));
}

/*============================================================================*
 * find_home is given an index of a variable in vtbl.  It attempts to find
 *      a home for the variable if it doesn't have one already.
 *============================================================================*/

static int
find_home(int v)
{
    int   home;

    if (vtbl[v].home)
	return vtbl[v].home;
    else if (vtbl[v].target && free_target(vtbl[v].target))
	home = vtbl[v].target;
    else
	home = find_temp();
    model[home] = MK_VO(v);
    vtbl[v].home = home;
    return home;
}

/*============================================================================*
 * find_temp looks first for a machine temporary to use and then on the
 *      stack between SP and Adst for a location to use.  If it can't find
 *      one this way, it decrements SP and uses the top of stack.  find_temp
 *      does not initialize the location in the model.  The caller is
 *      expected to perform this function in order to complete the allocation.
 *============================================================================*/

int
find_temp(void)
{
    int   i;

    for (i = TSTART; i <= TEND; i++)
	if (model[i] == NIL_VAL)
	    return (i);

    for (i = model_SP; i < model_SPstart; i++)
	if (model[i] == NIL_VAL)
	    return (i);

    /*---------------------------------------------------------------------*
     * No temporaries available.  Allocate a new stack location.
     *---------------------------------------------------------------------*/

    icode(I_ADDTOSP, -1, 0, 0, 0);
    model_SP = model_SP - 1;
    model[model_SP] = NIL_VAL;	/* initialize new location */

    return (model_SP);
}

/*============================================================================*
 * find_temp_in_reg attempts to find a machine temporary to use.  It will
 *      return zero if it can not find one.  If the parameter targ is a register
 *      which is free, then find_temp_in_reg will allocate it there.
 *============================================================================*/

static int
find_temp_in_reg(int targ)
{
    int   i;

    if (targ && targ <= LASTREG && model[targ] == NIL_VAL)
	return targ;

    for (i = TSTART; i <= TEND; i++)
	if (model[i] == NIL_VAL)
	    return i;

    return 0;
}

/*============================================================================*
 * free_target(loc)
 *
 *      This function is given a location in the model (loc) to free.
 *      It will do its level best to free up the location if it is not already
 *      free.  If the location is already free or can be freed, it returns 1.
 *      Otherwise it returns 0.
 *============================================================================*/

static int
free_target(int loc)
{
    int   start, tdbase, loc2;
    int   limit, v, dst;

    TDT_SET(tdbase, 0);
    start = loc;

    for (;;) {

	/*---------------------------------------------------------------------*
	 * Set loc2 and limit depending on the situation
	 *---------------------------------------------------------------------*/

	if (ASTART <= loc && loc <= AEND) {
	    limit = ASTART + nargsmatched;
	    loc2 = loc;
	}
	else if (loc <= LASTREG) {
	    limit = LASTREG;
	    loc2 = loc;
	}
	else if (loc >= model_E) {
	    limit = model_E + 2 + nargsmatched;
	    loc2 = loc;
	}
	else if (model_Adst2 && loc >= model_Adst - 2) {
	    limit = model_E + 2 + nargsmatched;
	    loc2 = (loc - model_Adst) + model_Adst2;
	}
	else {
	    limit = MODELSIZE;
	    loc2 = loc;
	}

	if (loc != loc2) {
	    if (IS_VO(model[loc])) {
		v = VO_VAL(model[loc]);
		if (vtbl[v].pvnum)
		    move_perms(loc);
		else if (vtbl[v].home == loc)
		    move_to_temp(loc);
	    }
	}

	if (model[loc2] == NIL_VAL)
	    break;
	else if (IS_VO(model[loc2])
		 && vtbl[v = VO_VAL(model[loc2])].home == loc2) {
	    if (vtbl[v].pvnum) {
		move_perms(loc2);
		break;
	    }
	    else if (vtbl[v].home == vtbl[v].target)
		break;
	    else if (vtbl[v].target && vtbl[v].target != start) {
		to_do[to_do_top++] = loc;
		to_do[to_do_top++] = loc2;
		loc = vtbl[v].target;
	    }
	    else {
		move_to_temp(loc2);
		break;
	    }
	}
	else if (loc2 < limit)
	    break;
	else if (loc == start)
	    return 0;		/* There is nothing on the stack */
	else {
	    loc2 = to_do[--to_do_top];
	    loc = to_do[--to_do_top];
	    move_to_temp(loc2);
	    break;
	}
    }

    while (to_do_top > tdbase) {
	dst = loc;
	loc2 = to_do[--to_do_top];
	loc = to_do[--to_do_top];
	v = VO_VAL(model[loc]);
	if (loc == loc2 && IS_VO(model[dst]) && v == VO_VAL(model[dst])) ;
	/* variable is already in place */
	else {
	    move(loc2, dst);
	    model[dst] = model[loc2];
	    model[loc2] = NIL_VAL;
	}
	vtbl[VO_VAL(model[dst])].home = dst;
    }
    return 1;
}

static void
move_perms(int start)
{
    int   i;

    for (i = start; i >= model_Eend; i--)
	if (IS_VO(model[i])) {
	    int   v = VO_VAL(model[i]);

	    if (vtbl[v].pvnum && vtbl[v].home == i) {
		int   t;

		if ((t = vtbl[v].target) &&	/* if there's a target */
		    model[t] == NIL_VAL &&
		    (model_Adst2 == 0 ||
		     model[model_Adst2 + t - model_Adst] == NIL_VAL)) {
		    ;		/* do nothing....t is already set by the if */
		}
		else
		    t = find_temp();
		if (vtbl[v].unsafe && vtbl[v].firstocc != 0) {
		    icode(I_P_UNSAFE, index_of(i), disp_of(i),
			  index_of(t), disp_of(t));
		    vtbl[v].unsafe = 0;
		}
		else
		    move(i, t);
		model[t] = model[i];
		vtbl[v].home = t;
		vtbl[v].pvnum = 0;	/* no longer permanent */
		if (i != t)
		    model[i] = NIL_VAL;
	    }
	}
}

static void
move_to_temp(int loc)
{
    int   v = VO_VAL(model[loc]);
    int   t = find_temp();

    model[t] = model[loc];
    vtbl[v].home = t;
    move(loc, t);
    model[loc] = NIL_VAL;
}

/*============================================================================*
 * comp_goal is responsible for setting up arguments for a goal.  The
 *      strategy used here is to set up the non-variable parts of the
 *      first in the hope that by the time we get done setting up the
 *      structure, the variables will be set up in the appropriate locations.
 *============================================================================*/

static void
comp_goal(pword goal)
{
    int   i, n, v, loc;
    pword arg;

    if (TYPEOF(goal) != TP_TERM)
	return;

    n = TERM_ARITY(goal);

    /*
     * compile non-variable parts
     */

    for (i = 1; i <= n; i++) {
	if (i <= NAREGS) {
	    loc = ASTART + i - 1;
	}
	else {
	    loc = model_Adst + i - 1;
	}
	arg = TERM_ARGN(goal, i);
	switch (TYPEOF(arg)) {
	    case TP_INT:
		free_target(loc);
		icode(I_P_INT, INT_VAL(arg), index_of(loc), disp_of(loc), 0);
		model[loc] = arg;
		break;
	    case TP_SYM:
		free_target(loc);
		icode(I_P_SYM, FUNCTOR_TOKID(arg), index_of(loc), disp_of(loc), 0);
		model[loc] = arg;
		break;
	    case TP_UIA:
	    case TP_LIST:
	    case TP_TERM:
#ifdef DoubleType
	    case TP_DOUBLE:
#endif /* DoubleType */
		comp_goal_structure(arg, loc);
		break;
	    default:
		break;
	}
    }

    /*
     * set up variables
     */

    for (i = 1; i <= n; i++) {
	arg = TERM_ARGN(goal, i);
	if (i <= NAREGS) {
	    loc = ASTART + i - 1;
	}
	else {
	    loc = model_Adst + i - 1;
	}
	if (IS_VO(arg)) {
	    v = VO_VAL(arg);
	    if (!(IS_VO(model[loc]) && VO_VAL(model[loc]) == v)
		|| vtbl[v].pvnum) {

		/* There is something to do if the variable is not already
		 * at home or if it is at home, but is still a permanent
		 * variable.
		 */
		free_target(loc);

		if (vtbl[v].usecnt == 0) {
		    if (vtbl[v].pvnum) {
			icode(I_P_YVAR,
			      index_of(vtbl[v].home),
			      disp_of(vtbl[v].home),
			      index_of(loc),
			      disp_of(loc));
		    }
		    else {
			icode(I_P_XVAR, index_of(loc), disp_of(loc), 0, 0);
			vtbl[v].home = loc;
			vtbl[v].unsafe = 0;
		    }
		    vtbl[v].usecnt++;
		}
		else {
		    int   src = vtbl[v].home;

		    if (vtbl[v].pvnum && vtbl[v].lastocc == goalnumber
			&& vtbl[v].firstocc != 0
			&& vtbl[v].unsafe) {
			icode(I_P_UNSAFE, index_of(src),
			      disp_of(src),
			      index_of(loc),
			      disp_of(loc));
			vtbl[v].pvnum = 0;
			vtbl[v].home = loc;
			model[loc] = MK_VO(v);
			vtbl[v].usecnt++;
			if (loc != src)
			    model[src] = NIL_VAL;
		    }
		    else {
			move(src, loc);
			model[loc] = model[src];
			vtbl[v].usecnt++;
			if (vtbl[v].lastocc == goalnumber) {
			    vtbl[v].home = loc;
			    if (src != loc && vtbl[v].pvnum)
				model[src] = NIL_VAL;
			    vtbl[v].pvnum = 0;	/* convert it to a temporary */
			}
		    }
		}
	    }
	    else
		vtbl[v].usecnt++;	/* somebody else did the move for us, but
					 * they didn't update the use count
					 */
	}
    }
}

#ifdef NewMath
/*============================================================================*
 * comp_math_struct is called by comp_math.c when the math compiler finds
 * something which it does not recognize.  When this happens, structure
 * is built and placed at where SP points.  Any temps allocated on the stack
 * are cleaned up after the argument is built.
 *============================================================================*/

void
comp_math_struct(pword arg)
{
    int   SPstart, loc;

    icode(I_MTH_CALLOUT_INIT, 0, 0, 0, 0);
    /* following two lines added by Kev, 5-20-93 */
    icode(I_ADDTOSP, -1, 0, 0, 0);
    model_SP--;

    SPstart = loc = model_SP;
    switch (TYPEOF(arg)) {
	case TP_INT:
	    free_target(loc);
	    icode(I_P_INT, INT_VAL(arg), index_of(loc), disp_of(loc), 0);
	    model[loc] = arg;
	    break;
	case TP_SYM:
	    free_target(loc);
	    icode(I_P_SYM, FUNCTOR_TOKID(arg), index_of(loc), disp_of(loc), 0);
	    model[loc] = arg;
	    break;
	case TP_UIA:
	case TP_LIST:
	case TP_TERM:
#ifdef DoubleType
	case TP_DOUBLE:
#endif /* DoubleType */
	    comp_goal_structure(arg, loc);
	    break;
	default:
	    break;
    }
    /* reset model_SP and top to original condition */
    icode(I_ADDTOSP, SPstart - model_SP, 0, 0, 0);
    model_SP = SPstart;
    model[model_SP] = NIL_VAL;
    /* MTH_CALLOUT will pop stack element off, i.e */
    icode(I_MTH_CALLOUT, 0, 0, 0, 0);
    /* following line added (5-20-93) to reflect popping by MTH_CALLOUT */
    model_SP++;

}
#endif

/*============================================================================*
 * comp_goal_structure is responsible for emitting the code for building
 *      structure in a goal.  The arguments to this function are:
 *
 *      strct   --      structure to emit code for
 *      loc     --      location to put the structure in.  If this value
 *                      is zero, then comp_goal_structure will allocate
 *                      a temporary and put the structure in this temporary.
 *                      In either event, comp_goal_structure will return
 *                      the location in which it placed the built structure
 *
 *============================================================================*/

static int
comp_goal_structure(pword strct, int loc)
{
    int   i, n;
    int   tdbase;
    int   tp;

    tp = TYPEOF(strct);

    if (tp == TP_UIA
#ifdef DoubleType
	|| tp == TP_DOUBLE
#endif
	) {
	TDT_SET(tdbase, 0);
	if (loc == 0)
	    loc = find_temp();
	else
	    free_target(loc);
	model[loc] = strct;
	if (tp == TP_UIA)
	    icode(I_P_UIA, (long) UIA_NAME(strct),
	          index_of(loc), disp_of(loc), 0);
#ifdef DoubleType
	else
	    icode(I_P_DBL, DOUBLE_VAL1(strct), DOUBLE_VAL2(strct),
		  index_of(loc), disp_of(loc));
#endif
    }
    else if (tp == TP_LIST) {
	n = 2;
	TDT_SET(tdbase, n);
	to_do[tdbase + 1] = comp_struct_arg1(LIST_CDR(strct));
	to_do[tdbase] = comp_struct_arg1(LIST_CAR(strct));
	if (loc == 0)
	    loc = find_temp();
	else
	    free_target(loc);
	model[loc] = strct;
	icode(I_P_LIST, index_of(loc), disp_of(loc), 0, 0);
	comp_struct_arg2(LIST_CAR(strct), to_do[tdbase], 0);
	comp_struct_arg2(LIST_CDR(strct), to_do[tdbase + 1], 1);
	icode(I_ENDSTRUCT, 0, 0, 0, 0);
    }
    else {
	n = TERM_ARITY(strct);
	TDT_SET(tdbase, n);
	for (i = n; i > 0; i--)
	    to_do[tdbase + i - 1] = comp_struct_arg1(TERM_ARGN(strct, i));
	if (loc == 0)
	    loc = find_temp();
	else
	    free_target(loc);
	model[loc] = strct;
	icode(I_P_STRUCTURE, functor_id_of_term(strct), arity_of_term(strct),
	      index_of(loc), disp_of(loc));
	for (i = 1; i <= n; i++)
	    comp_struct_arg2(TERM_ARGN(strct, i), to_do[tdbase + i - 1], i - 1);
	icode(I_ENDSTRUCT, 0, 0, 0, 0);
    }

    TDT_RESET(tdbase);
    return loc;
}

static int
comp_struct_arg1(pword arg)
{
    switch (TYPEOF(arg)) {
	case TP_UIA:
	case TP_LIST:
	case TP_TERM:
#ifdef DoubleType
	case TP_DOUBLE:
#endif
	    return (comp_goal_structure(arg, 0));
	    break;
	default:
	    return 0;
	    break;
    }
}

static void
comp_struct_arg2(pword arg, int loc, int n)
{
    int   t, v;

    if (loc != 0) {
	icode(I_U_VAL, index_of(loc), disp_of(loc), n, 0);
	model[loc] = NIL_VAL;	/* all done with temporary */
    }
    else {
	switch (TYPEOF(arg)) {

	    case TP_INT:
		icode(I_U_INT, INT_VAL(arg), n, 0, 0);
		break;
	    case TP_SYM:
		icode(I_U_SYM, FUNCTOR_TOKID(arg), n, 0, 0);
		break;
	    case TP_VO:
		v = VO_VAL(arg);
		if (vtbl[v].noccurences == 1) {
		    icode(I_U_VOID, n, 0, 0, 0);
		    vtbl[v].usecnt++;
		}
		else {
		    t = find_home(v);
		    if (vtbl[v].usecnt == 0) {
			icode(I_U_VAR, index_of(t), disp_of(t), n, 1);
			vtbl[v].unsafe = 0;
		    }
		    else if (vtbl[v].unsafe)
			icode(I_U_LVAL, index_of(t), disp_of(t), n, 0);
		    else
			icode(I_U_VAL, index_of(t), disp_of(t), n, 0);
		    vtbl[v].usecnt++;
		    if (vtbl[v].usecnt == vtbl[v].noccurences)
			model[t] = NIL_VAL;
		}

		break;
	    default:
		printf("Something funny in comp_struct_arg2\n");
		break;
	}
    }
}

/*============================================================================*
 * get_compiler_directives
 *
 *      We at ALS are starting to write some clause transformers which will
 *      produce more efficent code.  Occasionally we need to have the compiler
 *      do something for us in a somewhat explicit manner.
 *
 *      We arrange to do this by constructing a chain of als$cd structures
 *      the first argument is an integer representing the compiler directive
 *      and the second argument points to the rest of the chain (terminated
 *      by the goal to run).  Any additional arguments represent arguments
 *      to the compiler directive.
 *
 *      Compiler Directives
 *      -------------------
 *
 *      Code    N Args  Description
 *      ----    ------  -----------
 *         0         1  puts the current cutpoint in the variable indicated
 *                      by its single argument
 *
 *      get_compiler_directives will return the actual goal to compile
 *      (the thing at the end of the chain) after setting variables which
 *      indicate the directives required.
 *============================================================================*/

static pword
get_compiler_directives(pword goal)
{
    pword arg;

    while (TYPEOF(goal) == TP_TERM
	   && FUNCTOR_TOKID(TERM_FUNCTOR(goal)) == TK_ALSCD) {
	arg = TERM_ARGN(goal, 1);
	if (TYPEOF(arg) == TP_INT) {
	    switch (INT_VAL(arg)) {
		case 0:	/* get cutpt */
		    arg = TERM_ARGN(goal, 3);
		    if (TYPEOF(arg) == TP_VO)
			cd_cutpt = VO_VAL(arg);
		    break;
		default:
		    break;
	    }
	}
	goal = TERM_ARGN(goal, 2);
    }

    while (TYPEOF(goal) == TP_TERM &&
	   FUNCTOR_TOKID(TERM_FUNCTOR(goal)) == TK_COLON &&
	   TERM_ARITY(goal) == 2 &&
	   TYPEOF((arg = TERM_ARGN(goal, 1))) == TP_SYM &&
	   TYPEOF(TERM_ARGN(goal, 2)) != TP_VO) {
	cd_gmod = FUNCTOR_TOKID(arg);
	goal = TERM_ARGN(goal, 2);
    }
    return goal;
}

static void
compiler_directives(void)
{
    if (cd_cutpt != -1)
	emit_cd_cutpt();
}

static void
emit_cd_cutpt(void)
{
    int   loc = find_home(cd_cutpt);

    vtbl[cd_cutpt].usecnt++;
    vtbl[cd_cutpt].unsafe = 0;
    icode(I_CUTMACRO, EREG, MODELSIZE - 1 - model_E, index_of(loc), disp_of(loc));
    cd_cutpt = -1;
}
