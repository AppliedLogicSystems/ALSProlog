/*
 * parsstak.h       -- parser stack defines
 *      Copyright (c) 1985 by Kevin A. Buettner
 *	Copyright (c) 1990-1993 Applied Logic Systems, Inc.
 *
 * Author:  Kevin A. Buettner
 * Creation Date: 8/8/85
 * Revision History:
 *	Revised: 12/21/90,	kev	-- stack split into operator/operand
 *					   stack
 *
 *
 * Explanation:
 *      The parser stack is quite a handy thing to use not only in the parser,
 *      but also in the expansion/translation functions which convert terms
 *      to other more usable objects.   expand.c and parser.c are presently
 *      the only modules which use these defines.
 */


#define PSTKSZ 850

struct rator {
	pword	*last_rand;	/* pointer to last operand on operand stack */
	long	precedence;
	long	token_id;
};

extern struct rator *pst_rator;	/* parser stack top -- rator (operator) */
extern pword *pst_rand;		/* parser stack top -- rand */

#define TOP_RATOR (*(pst_rator-1))
#define TOP_RAND (*(pst_rand-1))
