/*===================================================================*
 |		parsstak.h       
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-95 Applied Logic Systems, Inc.
 |
 |		-- parser stack defines
 |
 | Author:  Kevin A. Buettner
 | Creation Date: 8/8/85
 | 12/21/90 - K.Buettner -- stack split into operator/operand stack
 |
 | Explanation:
 |      The parser stack is quite a handy thing to use not only in the parser,
 |      but also in the expansion/translation functions which convert terms
 |      to other more usable objects.   expand.c and parser.c are presently
 |      the only modules which use these defines.
 *===================================================================*/

#ifdef KERNAL
#define PSTKSZ 150
#else
#define PSTKSZ 850
#endif /* KERNAL */

struct rator {
	pword	*last_rand;	/* pointer to last operand on operand stack */
	long	precedence;
	long	token_id;
};

//extern struct rator *pst_rator;	/* parser stack top -- rator (operator) */
//extern pword *pst_rand;		/* parser stack top -- rand */

#define TOP_RATOR (*(pst_rator-1))
#define TOP_RAND (*(pst_rand-1))
