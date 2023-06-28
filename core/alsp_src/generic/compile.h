/*================================================================*
 |			compile.h
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1989-1993 Applied Logic Systems, Inc.
 |
 |			-- definitions for compiler
 |
 | This file describes to the compiler how Prolog maps onto particular
 | machine architectures.
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/16/85
 | 02/7/89 - K.Hughes -- Changed for 386 compiler mods
 | 05/1/89 - K.Buettner -- 88k and sun version merged with 386
 | 03/7/90 - K.Buettner	-- added in SPARC defns
 *================================================================*/

#ifdef KERNAL
#define TODOSIZE  2048
#define MODELSIZE 1024
#else
#define TODOSIZE  16384
#define MODELSIZE 8192
#endif /* KERNAL */

/*
 * Register Base Numbers
 *	SPREG		-- stack pointer
 *	EREG		-- environment pointer
 */

#ifdef Portable

#define REGS    0
#define SPREG   1
#define EREG    2

#define ASTART  0
#define AEND    -1
#define TSTART  0
#define TEND    -1
#define LASTREG 0

#define STACKADJUST 2

#endif /* Portable */

#ifdef arch_m88k

#define REGS	0
#define SPREG	23
#define EREG	30 

#endif


#ifdef arch_m68k

#define REGS	0
#define SPREG	7
#define EREG	6

#endif

#ifdef arch_i386

#define REGS	0
#define SPREG	1
#define EREG	2

#endif

#ifdef arch_sparc

#define REGS	0
#define SPREG	22
#define EREG	13

#endif	/* arch_sparc */

#ifdef arch_vax

#define REGS 0
#define SPREG	14
#define EREG 	12

#endif /* arch_vax */

/*
 * CPREG and OLDEREG.
 *
 * CPREG is the machine register used for the continuation pointer.
 * OLDEREG is the machine register used to hold the old environment pointer.
 *
 *
 * If no registers are available for either of these, there should be no 
 * #define for them.  If there is, the compiler will produce incorrect code.
 */

#ifdef arch_m88k

#define CPREG	15	/* Continuation pointer */
#define OLDEREG	14	/* Old Env pointer	*/

#endif /* arch_m88k */


#ifdef arch_sparc

#define CPREG 19
#define OLDEREG 18

#endif /* arch_sparc */

/*
 * CPREG and OLDEREG are not defined for the m68k, i386, and vax versions.
 */


/*
 * Where things live in the low end of the model
 *
 * ASTART and AEND mark where the argument registers that actually live in
 * machine registers will be placed. ASTART marks the first register and AEND
 * marks the last one.  It is assumed that the registers are in consecutive 
 * order.
 *
 * When changing these, to eliminate arguments being put into registers,
 * I recommend setting AEND to -1 and ASTART to 0.  This will make the NAREGS
 * macro work out right below.
 *
 * TSTART and TEND mark which machine registers can be used for temporary
 * variable storage. TSTART marks the first register and TEND marks the
 * last one.  It is assumed that the registers are in consecutive order.
 *
 * When changing these to eliminate temporary machine registers, I recommend
 * setting TEND to -1 and TSTART to 0.  This makes the NTREGS macro work out
 * right below.
 *
 *
 * LASTREG is the last machine register on the processor (which the compiler
 * models).
 */


#ifdef arch_m88k

#define ASTART	2	/* first arg reg	*/
#define AEND	4	/* last arg reg		*/
#define TSTART	6	/* first temp reg	*/
#define TEND	9	/* last temp reg	*/

#define LASTREG	31	/* The last register in the model */

#endif

#ifdef arch_m68k


/*
 * Low end of the model for the 020:
 *
 *	No A registers.
 *	Four temporaries in D1-D4.
 */

#define ASTART	0
#define AEND	-1
#define TSTART	1
#define TEND	4
#define LASTREG	7

#endif

#ifdef arch_i386

#define ASTART	0
#define AEND	-1
#define TSTART	0
#define TEND	-1
#define LASTREG	7

#endif


#ifdef arch_sparc

#define ASTART	24
#define AEND	26
#define TSTART	27
#define TEND	29
#define LASTREG 31

#endif /* arch_sparc */

#ifdef arch_vax

#define ASTART	0
#define AEND	-1
#define TSTART	3
#define TEND	3
#define LASTREG 14

#endif /* arch_vax */



/*
 * STACKADJUST is the value used to adjust the stack for the initial ALLOCATE
 * instruction in a multi-goal clause, or any stack adjustments before a CALL
 * instruction.
 */

#ifdef arch_m88k

#define STACKADJUST 2

#endif

#ifdef arch_m68k


/*
 * STACKADJUST is 0 for the 020 due to the fact that link and call
 * push their values onto the stack.
 */

#define STACKADJUST 0

#endif

#ifdef arch_i386
/*
 * STACKADJUST is 0 for the 386 due to the fact that link and call
 * push their values onto the stack.
 */

#define STACKADJUST 0

#endif /* arch_i386 */


#ifdef arch_sparc

#define STACKADJUST 2

#endif /* arch_sparc */


#ifdef arch_vax

#define STACKADJUST 0

#endif /* arch_vax */


/*
 * Calculate the number of argument registers (the K of the model) we have
 */

#define NAREGS ((AEND-ASTART)+1)

/*
 * The number of temporary registers which we have
 */

#define NTREGS ((TEND-TSTART)+1)



#ifdef arch_i386

/* Provide the mapping from compiler register definitions to Prolog machine
 * registers.  All of the above definitions are offsets into this array.
 * In the other implementations, the offsets are the actual register numbers.
 */

#define BASEREGS	{H_REG, SP_REG, E_REG }

#endif

/* compile.c */
extern	int	compile_clause	( pword, int );
extern	void	gccallinfo	( void );
extern	int	index_of	( int );
extern	int	disp_of		( int );
extern	int	find_temp	( void );
#ifdef NewMath
extern	void	comp_math_struct ( pword );
#endif

/* compmath.c */
extern	void	comp_math	( pword, int, long, long );
