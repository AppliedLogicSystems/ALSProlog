/*===============================================================*
 |			varproc.h            
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 Applied Logic Systems, Inc.
 |
 |			-- data structure definitions for dealing with
 |             variable allocation during compilation
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/16/85
 *===============================================================*/
#ifdef KERNAL
#define VTBLSIZE 256
#define MAXGLS 64
#else
#define VTBLSIZE 1024
#define MAXGLS 512
#endif

typedef struct {
            short firstocc;		/* goal in which var first appears 		*/
            short istoplevinhead; 
								/* 1 if variable occurs at top level in	the head */
            short lastocc;		/* goal in which var appears last       */
            short noccurences;	/* total number of occurences           */
            short pvnum;		/* 0 if temp, Y number otherwise        */
            short home;			/* place where variable is living		*/
            short usecnt;		/* usage count when compiling			*/
            short target;		/* target register to allocate for		*/
                            	/* temporary variables, if possible		*/
            short unsafe;    	/* used for unify_local_value			*/
         } varinf;

extern varinf vtbl[];          /* variable information table   			*/

extern int call_env_sizes[];	/* size of environment for each call (goal) in body */
#define IS_VO(s) (TYPEOF(s) == TP_VO)

/*
 * The variable table is actually two bigger than VTBLSIZE in order to
 * accomodate the return address and previous environment value.
 */

#define RETIDX (VTBLSIZE+1)
#define ENVIDX VTBLSIZE

/* ----------- varproc.c ----------- */
extern	int		classify_vars		( pword );
extern	void	compute_call_env_sizes	( int, int );

/* ----------- compile.c ----------- */
#ifdef NewMath
extern	int	isarithmetic		( pword );
#endif
