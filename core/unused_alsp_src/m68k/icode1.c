/*
 * icode1.c		-- stuff to emit instructions
 *	Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/12/87
 * Revision History:
 * 03/22/87 - K.Buettne -- icode.c split into icode1.c,
 *						   icode2.c, and icode.h
 * 10/26/94 - C. Houpt	-- Redefined the Instruction Pointer as
 *						   a union so that it can be used as both a
 *						   short and long pointer.  This avoid the
 *						   need to casting l-values, which is not ANSI.
 *						   Fixed similar problem in ic_uiastr().
 *						-- Put some obp code under control of OBP.
 *						-- Allocate large global arrays with malloc() for
 *						   Mac compilers that can't handle them.
 */

#include <stdio.h>

#include "defs.h"
#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "varproc.h"
#include "module.h"
#include "machinst.h"
#include "rinfo.h"


/*
 * READMODE, WRITEMODE, CAPTUREMODE, MACROMODE, and ALLOCMODE are the
 * potential values which the variable capturemode may take on.
 */

#define READMODE 0		/* in read mode			*/
#define WRITEMODE 1		/* in write mode (the default)	*/
#define CAPTUREMODE 2		/* capturing unify instrs for	*/
				/* read/write mode expansion	*/
#define MACROMODE 3		/* loading a macro expansion	*/
#define ALLOCMODE 4		/* emitting allocation code	*/

/*
 * MAXCALLS is the maximum number of goals which a clause can have.  This
 * constant will depend on the value of MAXGLS from varproc.h
 */

#define MAXCALLS		MAXGLS-1


/*
 * CAPTURESIZE is the size of the capture area.  The capture area is where we
 * put the icode instructions in a unify sequence in order to split the
 * instructions into read/write mode later on.
 *
 * There are other capture areas.  One is the macro area -- the area in which
 * different kinds of expansions are temorarily put (math code goes in this
 * area).  The other is the allocation area.  This area is where certain
 * allocation instructions are diverted for possible future duplication if
 * the clause requires it.
 */

#define CAPTURESIZE 500

/*
 * Capture (read/write mode) area
 */

#ifdef NO_FAR_DATA
static struct capturestruct {
		int iidx;
		int w,x,y,z;
	      } *capturearea;
#else
static struct capturestruct {
		int iidx;
		int w,x,y,z;
	      } capturearea[CAPTURESIZE];
#endif

static int captureidx;
static Code *capturepatch1;		/* patch indices for read/write mode */
static Code *capturepatch2;


/*
 * Macro area
 */

#ifdef NO_FAR_DATA
static struct capturestruct *macroarea;
#else
static struct capturestruct macroarea[CAPTURESIZE];
#endif
static int macroidx;
Code *ic_macropatch1;
Code *ic_macropatch2;


/*
 * Allocation area
 */

#ifdef NO_FAR_DATA
static struct capturestruct *allocarea;
#else
static struct capturestruct allocarea[CAPTURESIZE];
#endif
static int allocidx;

/*
 * Data structure for storing gc call information
 */

#ifdef NO_FAR_DATA
static struct {				/* data structure for storing	*/
		 Code *patchaddr;	/* gc call information		*/
		 long  argenvsize;
		 long  argmask;
	      } *callinfo;
#else
static struct {				/* data structure for storing	*/
		 Code *patchaddr;	/* gc call information		*/
		 long  argenvsize;
		 long  argmask;
	      } callinfo[MAXCALLS];
#endif
static int callidx;			/* call index			*/

#ifdef NO_FAR_DATA
void init_capturestructs(void)
{
    capturearea = malloc(CAPTURESIZE*sizeof(*capturearea));
    if (capturearea == NULL) fatal_error(FE_ALS_MEM_INIT, 0);
    macroarea = malloc(CAPTURESIZE*sizeof(*macroarea));
    if (macroarea == NULL) fatal_error(FE_ALS_MEM_INIT, 0);
    allocarea = malloc(CAPTURESIZE*sizeof(*allocarea));
    if (allocarea == NULL) fatal_error(FE_ALS_MEM_INIT, 0);
    callinfo = malloc(MAXCALLS*sizeof(*callinfo));
    if (callinfo == NULL) fatal_error(FE_ALS_MEM_INIT, 0);
}
#endif

/*
 * Other state variables
 */

static int firstargprocessed;		/* 1 if the first argument has been
					 * processed.  0 otherwise.
					 */
static Code *dstart;			/*
					 * Pointer to the determinate case
					 * when we don't know anything about
					 * the first argument
					 */

static Code *firstargptr;		/*
					 * Pointer to the determinate code
					 * for the first argument in which
					 * the dereference code is skipped
					 * because A1 has the dereferenced
					 * first argument.
					 */
				
static int capturemode = WRITEMODE;


/*
 * makeobp is a flag which indicates whether or not we are writing the
 * icode calls out to an obp file or not.
 */
#ifdef OBP
int makeobp;
#endif /* OBP */

/*
 * Forward declarations
 */

static	void	ic_replacebranch	PARAMS(( Code * ));
static	void	ic_uiastr		PARAMS(( char * ));
static	void	ic_p_const		PARAMS(( long, long, long ));
static	void	ic_1stargalloc		PARAMS(( void ));
static	void	ic_put_macro		PARAMS(( long ));
static	void	ic_g_const		PARAMS(( long, long, long, long ));
static	void	ic_u_const		PARAMS(( long, long ));
static	void	ic_backpatch		PARAMS(( Code * ));
static	void	ic_deref1		PARAMS(( int, Code ** ));
static	void	ic_begin_macro		PARAMS(( long, long, long, long ));
static	void	ic_end_macro		PARAMS(( long, long, long, long ));

#define ICODE(macro,str,addr,obp) void addr PARAMS(( long, long, long, long ));
#include "icodedef.h"
#undef ICODE

/*
 * Icode buffer:
 *	The icode buffer is a large block of memory allocated when the
 *	system starts up to hold clauses that the code generator is
 *	producing prior to putting them into the code area.  If it
 *	were possible to know in advance how much space is required for
 *	the clause, we could dispense with this buffer and simply allocate
 *	a chunk of clause space big enough for the clause.
 *
 *	icode_buf is a pointer to the start of the buffer.  Once set, it
 *	should never be changed.  ic_ptr, on the other hand, will change
 *	as code is emitted.  The ICBUF_OVERFLOW_CHECK macro is used to
 *	detect overflow and signal a fatal error if overflow has occurred.
 *
 *	init_icode_buf is an initialization function called in main.c
 *	to establish the icode buffer.  The icode buffer size may be
 *	changed with the -c command line switch.
 */

Code *icode_buf;

/*  Code *ic_ptr;  */
/* Define the Instruction Pointer (uip) as a union so that it can be used
   as both a short and long pointer.
*/
ic_uptr_type ic_uptr;

#define ICBUFSAFETY	0x100		/* 256 code words */

static Code *icode_buf_end;

int
init_icode_buf(size)
    int size;
{
    if (size < MIN_ICBUFSIZE)
	size = MIN_ICBUFSIZE;
    else if (size > MAX_ICBUFSIZE)
	size = MAX_ICBUFSIZE;
    
    if ((icode_buf = (Code *) malloc(size*sizeof(Code))) == 0)
	return 0;
    else {
	icode_buf_end = icode_buf + (size - ICBUFSAFETY);
	return 1;
    }
}

#define ICBUF_OVERFLOW_CHECK					\
	if (ic_ptr > icode_buf_end)				\
		fatal_error(FE_ICODEBUFOVER,0);

#define ICBUF_OVERFLOW_CHECK1(sz)					\
	if ((ic_ptr+(sz)) > icode_buf_end)				\
		fatal_error(FE_ICODEBUFOVER,0);


/*
 * ic_backpatch is used to fix up the byte displacement of a BRA or Bcc
 * 	instruction.  If using a byte displacement can't be used because the
 * 	forward reference is too far away, the instructions below the branch
 * 	instruction are shifted forward in memory by one word and a word 
 * 	displacement is installed.  The single parameter where is the index 
 * 	of the word after the branch instruction.  This is the location to 
 * 	install the word displacement, if necessary.  Fixing up a byte 
 * 	displacement involves backing up one word and oring in the displacement.
 * 	Note that we assume that the byte displacment starts out as zero which 
 * 	is necessary for either the short fixup or the word fixup.
 */

static void
ic_backpatch(where)
    Code *where;
{
    int disp = (ic_ptr-where)*2;
    if (disp <= 127) {
      	*(where-1) |= disp;		/* or in byte displacement */
    }
    else {	/* need to shift and make room for word displacement */
      	Code *ip;
      	for (ip=ic_ptr; ip >= where; ip--)
	    *(ip+1) = *ip;

	RELOC_UPDATE(where,sizeof(Code))

      	ic_ptr++;
      	*where = disp+2;
    }
}


/*
 * ic_deref1 is used to generate a dereference loop for arguments which we
 * 	assume are not self-referential (they may be, but then the loop will do
 * 	extra work). The pp parameter is the patch pointer.
 */

static void
ic_deref1(n,pp)
   int n;		/* number of address register to use in dereferencing */
   Code **pp;		/* patch pointer */
{
    ic_put(0x2040 | (n<<9));		/* movea.l D0, An */
    ic_put(0x0240);			/* and.w #3, D0   */
    ic_put(3);				/*       ^^       */
    ic_put(0x6600);			/* bne	disp (to be patched) */
    *pp = ic_ptr;
    ic_put(0x2010 | n);			/* move.l (An), D0 */
    ic_put(0xb088 | n);			/* cmp.l An, D0 */
    ic_put(0x66f2);			/* bne (start of this sequence) */
}


/*
 * Instruction:	ic_addtosp
 * Function:	adds a number to the stack pointer
 * Parameters:	num	-- number to add
 * Code summary:
 *	add.l	#num, SP
 */

void
ic_addtosp(num, x, y, z)
   long num; 
   long x, y, z;
{
   num *= 4;
   if (num != 0) {
      if (num < 0 && num >= -8) 	/* use subq */
	 SUBQ(-num, SP, ADIRECT, 0)
      else if (num >0 && num <= 8)	/* use addq */
	 ADDQ(num, SP, ADIRECT, 0)
      else {				/* else add immediate to address reg */
	 ADDIAW(num,SP)		/*  using a word operand	*/
      }
   }
}


/*
 * Instruction:	ic_call
 * Function:	implements the procedure call
 * Parameters:	p	-- token index of procedure to call
 *		a	-- arity of procedure
 * Code summary:
 *	call	p/a
 */

void
ic_call(p, a, y, z)
    long p, a;
    long y, z;
{
    ntbl_entry *ent;

    ent = w_nameentry(cur_mod,p,a);
    JSR(((long) ent->call_entry))

    RELOC_INFO(RELOC_PROC_CALL,(ic_ptr-(sizeof(long)/sizeof(Code))),0)
}



/*
 * Instruction: ic_execute
 * Function:	implements the execute instruction (call of last goal)
 * Parameters:	p	-- token index of procedure to execute
 *		a	-- arity of procedure
 * Code summary:
 *	jmp	p/a
 */

void
ic_execute(p, a, x, y)
    long p, a;
    long x, y;
{
    Code *ptr = ic_ptr;
    ntbl_entry *ent;

    ent = w_nameentry(cur_mod,p,a);

    MOVE(SP,ADIRECT,0, E,ADIRECT,0)
    JMP(ent->exec_entry)
 
    RELOC_INFO(RELOC_PROC_EXEC,(ic_ptr-(sizeof(long)/sizeof(Code))),0)
 
    ic_replacebranch(ptr);
}


/*
 * Instruction: ic_allocate
 * Function:	allocates environment and space for first call in a multi-
 *		goal clause.
 * Parameters:	size	-- combined size of environment and arguments
 *				(in longwords)
 * Code Summary:
 *	link	E, #-4*(size-1)
 *	addq	#4, E
 *
 */

void
ic_allocate(size, x, y, z)
    long size;
    long x, y, z;
{
    ic_addtosp(-size,0,0,0);
}


/*
 * Instruction: ic_allocate1
 * Function:	allocates space for goal in a clause with one goal
 * Parameters:	size	-- space on stack needed in non-determinate case
 *			   (this is usually equal to the size of the
 *			    head)
 */

void
ic_allocate1(size, x, y, z)
    long size;
    long x, y, z;
{
    if (capturemode != READMODE)
	ic_addtosp(-size,0,0,0);
}

/*
 * Instruction:	ic_endallocate1
 * Function:	Puts down label (backpatches) for the ic_allocate1
 *		instruction.   See above.
 * Parameters:	none
 */

void
ic_endallocate1(x, y, z, w)
    long x, y, z, w;
{
    if (capturemode != READMODE) {
	dstart = ic_ptr;
    }
}


/*
 * Instruction:	ic_deallocate
 * Function:	Sets up stack pointer for last call in a multi-goal clause.
 *		Also restores previous environment pointer.
 * Parameters:	size1	-- value in longwords to subtract from AB giving
 *			   the new stack position when things are
 *			   non-determinate.
 *		size2	-- value in longwords to subtract from A when the
 *			   clause is determinate
 *		isdeterminateforsure
 *			-- 1 if the compiler knows for certain that the clause
 *			   is determinate at this point; 0 otherwise.
 */

/* 
 * Instruction: ic_deallocate1 through ic_deallocate4
 * Description: These four functions perform the same actions as ic_deallocate.
 */


/*
 * Instruction: ic_deallocate1 through ic_deallocate4
 * Description:	These four functions perform the same actions as ic_deallocate.
 */

static Code *deallocate2patch;
static Code *deallocate3patch;

void
ic_deallocate1(w,x,y,z)
    long w, x, y, z;
{
    deallocate3patch = (Code *) 0;
}

void
ic_deallocate2(size1,x,y,z)
    long size1;
    long x, y, z;
{
    CMPAD(E,SPB)
    BHI(0)
    LABEL(deallocate2patch)
    MOVE(SPB,DDIRECT,0,A0,ADIRECT,0)
    LEA(A0,DISPL,-4*size1,SP)
}

void
ic_deallocate3(w,x,y,z)
    long w, x, y, z;
{
    BRA(0)
    LABEL(deallocate3patch)
    ic_backpatch(deallocate2patch);
}

void
ic_deallocate4(size2,x,y,z)
    long size2;
    long x, y, z;
{
    if (size2 == 0) {
	MOVE(E,ADIRECT,0, SP,ADIRECT,0)
    }
    else {
	LEA(E,DISPL,-4*size2,SP)
    }
    if (deallocate3patch) 
	PATCHDISP(deallocate3patch)
}


/*
 * Instruction:	ic_trim
 * Function:	Sets up stack pointer for next call in a multi-goal clause
 *		This code is not used before the first or last goals.  It
 *		also trims some of the environment away when possible.
 * Parameters:	size1	-- value in longwords of environment that must
 *			   persist after the call.
 *		size2	-- value in longwords of size of next goal or
 *			   difference in size of environment previously
 *			   and current size which ever is greater
 *		isdeterminateforsure	
 *			-- 1 if the compiler knows for certain that the
 *			   clause is determinate at this point; 0 otherwise
 * Code Summary:
 *	Determinate case:
 *		lea	-(size1+size2)*4(E), SP
 *	Unknown case:
 *		move.l	SPB, A0
 *		cmp	E, SPB
 *		blo	1f
 *		lea	-size1*4(E), A0
 * 	1:	lea	-size2*4(A0), SP
 */


void
ic_trim(size1,size2,isdeterminateforsure, x)
    long size1, size2, isdeterminateforsure, x;
{
    int realsize1 = 4*size1;
    int realsize2 = 4*size2;

    if (isdeterminateforsure) {
	LEA(E,DISPL,-(realsize1+realsize2),SP)
    }
    else {
	Code *patch;
	MOVE(SPB,DDIRECT,0,A0,ADIRECT,0)
	CMPAD(E,SPB)
	BLO(0)
	patch = ic_ptr;
	if (realsize1 == 0) {
	    MOVE(E,ADIRECT,0, A0,ADIRECT,0)
	}
	else {
	     LEA(E,DISPL,-realsize1,A0)
	}
	PATCHDISP(patch)
	if (realsize2 == 0) {
	     MOVE(A0,ADIRECT,0, SP,ADIRECT,0)
	}
	else {
	     LEA(A0,DISPL,-realsize2,SP)
	}
    }
}

/*
 * Instruction: ic_proceed
 * Function:	Implements the return from procedure
 * Parameters:	base	-- base from which to get the return address
 *		disp	-- displacement off of SP of return address
 *			   (in longwords)
 * Code Summary:
 *	move.l	4*disp(SP), A0
 *	jmp	(A0)
 */

void
ic_proceed(base,disp,y,z)
    long base, disp, y, z;
{
    int edisp, mode, reg;

    edisp = disp-1;

    COMPUTE_MODE(base,disp,reg,mode)
    MOVE(reg,mode,disp,A0,ADIRECT,0)

    COMPUTE_MODE(base,edisp,reg,mode)
    MOVE(reg,mode,edisp, E,ADIRECT,0)
    ic_put(0x4ed0);		/* jmp (A0) */
}


/*
 * Instruction:	ic_inline_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *		usually is emitted as a result of a cut as the last goal.
 * Parameters:	none
 */

void
ic_inline_proceed(w,x,y,z)
    long w, x, y, z;
{
    MOVE(E,DISPL,4, A0,ADIRECT,0)
    MOVE(E,INDIRECT,0,E,ADIRECT,0)
    ic_put(047320);			/* jmp (a0) */
}

/*
 * Instruction: ic_g_uia
 * Function:	Emits code for matching a uia in the head
 * Parameters:	uiastr		-- string corresponding to uia
 *		base		-- index of base register
 *		disp		-- displacement from base register
 */

void
ic_g_uia(uiastr,base,disp,x)
    long uiastr;
    long base, disp;
    long x;
{
    int mode, reg;

    COMPUTE_MODE(base,disp,reg,mode);
    MOVE(reg,mode,disp,D0,DDIRECT,0)
    JSR((long) wm_g_uia);

    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_g_uia)
 
    ic_uiastr((char *)uiastr);
}

/*
 * Instruction: ic_p_uia
 * Function:	Emits code for putting down a uia in the body
 * Parameters:	uiastr		-- string corresponding to the UIA
 *		base		-- index of the base register
 *		disp		-- displacement from the base register
 *
 */

void
ic_p_uia(uiastr,base,disp,x)
    long uiastr;
    long base, disp;
    long x;
{
    int mode, reg;

    JSR((long) wm_p_uia);

    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_p_uia)
 
    ic_uiastr((char *)uiastr);
    COMPUTE_MODE(base,disp,reg,mode)
    MOVE(D0,DDIRECT,0,reg,mode,disp)
}

static void
ic_uiastr(s)
    char *s;
{
    register int l,i;
    l = strlen(s)+1;
    if (l & 3)
	l = (l & ~3) + 4;		/* round up */
   
    l += 4;				/* add one longword for the fence */
    l >>= 2;				/* convert to longwords		*/
    ic_put(l);				/* put down number of longwords-1 */
    ic_putl(MMK_FENCE(l));		/* put down the first fence	*/
    for (i=1; i<l; i++)
/*	ic_putl(*((long *) s)++);	* put down the string		*/  
    {
	ic_putl(*((long *) s));	/* put down the string		*/
	s += sizeof(long);
    }
    ic_putl(MMK_FENCE(l));		/* put down another fence	*/
}

/* 
 * Instruction ic_g_sym
 * Function:	Emits code corresponding to the symbol part of Warren's
 *		get_constant instruction
 * Parameters:	tokid	-- token index
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */

void
ic_g_sym(tokid,base,disp,x)
    long tokid, base, disp;
    long x;
{
    ic_g_const(MMK_SYM(tokid),base,disp,MTP_SYM);
}

/*
 * Instruction: ic_g_int
 * Function:	Emits code corresponding to the integer part of Warren's
 *		get_constant instruction
 * Parameters:	i	-- integer to get
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */

void
ic_g_int(i,base,disp,x)
    long i, base, disp;
    long x;
{
    ic_g_const(MMK_INT(i),base,disp,MTP_INT);
}


/*
 * Procedure:	ic_g_const
 * Function:	emits code appropriate for g_sym and g_int.
 * Parameters:	const	-- encoded constant representing the symbol or integer
 *		base	-- index of base register
 *		disp	-- displacement from base register
 *		tp	-- type of constant
 * Code Summary:
 *		move.l	disp(base), D0
 *
 *	1:	move.l	D0, A1
 *		and.w	#3, D0
 *		bne.s	2f
 *		move.l	(A1), D0
 *		cmp.l	A1, D0
 *		bne.s	1b
 *
 *		move.l	#<const>, (A1)
 *		cmp.l	A1, HB
 *		bls.s	3f
 *		cmp.l	A1, SPB
 *		bhi.s	3f
 *		move.l	A1, -(TR)
 *		bra.s	3f
 *
 *	2:	cmp.l	#<const>, A1
 *		beq.s	3f
 *  For integers,
 *		jmp	fail
 *	3:
 *  For symbols,
 *		move.l	A1, D0
 *		addq.l	#1, D0
 *		and.w	#0xf, D0
 *		beq.s	1f
 *		jmp	fail
 *	1:	move.l	A1, -(SP)
 *		move.l	#<const>, -(SP)
 *		jsr	cmp_sym_uia
 *		addq.l	#8, SP
 *		move.l	D0, D0
 *		bne.s	3f
 *		jmp	fail
 *	3:	
 */

static void
ic_g_const(constant,base,disp,tp)
    long constant, base, disp, tp;
{
    int reg,mode;
    COMPUTE_MODE(base,disp,reg,mode)
    MOVE(reg,mode,disp,D0,DDIRECT,0)

    if (-32768 < constant && constant < 32768) {
	MOVIW(constant,A0,ADIRECT,0)
    }
    else {
	MOVI(constant,A0,ADIRECT,0)
    }

    if (tp == MTP_INT) {
#ifndef PACKAGE
	JSRX(wm_g_int)
#else 	/* PACKAGE */
	JSR(wm_g_int)
	RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_g_int)
#endif 	/* PACKAGE */
    }
    else {
#ifndef PACKAGE
	JSRX(wm_g_sym)
#else 	/* PACKAGE */
	JSR(wm_g_sym)
	RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_g_sym)
#endif 	/* PACKAGE */
    }
}

/*
 * Instruction: ic_move
 * Function:	emits a move instruction
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary
 *		move.l	SourceEA,DestEA
 *
 * Note: if either sbase or dbase is TREG, then the displacement is the number
 *	 of the temporary to use.
 */

void
ic_move(sbase,sdisp,dbase,ddisp)
    long sbase, sdisp, dbase, ddisp;
{
    int smode,sreg;
    int dmode,dreg;
    COMPUTE_MODE(sbase,sdisp,sreg,smode)
    COMPUTE_MODE(dbase,ddisp,dreg,dmode)
    MOVE(sreg,smode,sdisp,dreg,dmode,ddisp)
}


/*
 * Instruction: ic_g_value
 * Function:	emits code to unify the two operands.
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note: If either sbase or dbase is TREG, then the displacement is the number
 *	 of the temporary to use.
 *
 * Code Summary:
 *		move	SEA, D0
 *		move	DEA, A0
 *		jsr	unify
 */

void
ic_g_value(sbase,sdisp,dbase,ddisp)
    long sbase, sdisp, dbase, ddisp;
{
    int sreg,smode,dreg,dmode;
    COMPUTE_MODE(sbase,sdisp,sreg,smode)
    COMPUTE_MODE(dbase,ddisp,dreg,dmode)
    MOVE(sreg,smode,sdisp,D0,DDIRECT,0)
    MOVE(dreg,dmode,ddisp,A0,ADIRECT,0)
#ifndef PACKAGE
    JSRX(UNIFY)
#else 	/* PACKAGE */
    JSR(UNIFY)
    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_unify)
#endif 	/* PACKAGE */
}

/*
 * Instruction: ic_g_list
 * Function:	emits code to unify an argument with a list
 * Parameters:	base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, then the displacement is the number of the 
 *	 temporary to use.
 *
 * Code summary:	(EA is formed with the appropriate addressing mode
 *			 involving base and disp)
 * 	Write mode:
 *		move.l	EA, D0
 *	1:	move.l	D0, A1
 *		and	#3, D0
 *		bne	3f
 *		move.l	(A1), D0
 *		cmp.l	A1, D0
 *		bne	1b
 *		lea	2(H), A0	;  2 is the list tag
 *		move.l	A0, (A1)	; bind variable
 *		cmp.l	A1, HB
 *		bls.s	2f
 *		cmp.l	A1, SPB
 *		bhi.s	2f
 *		move	A1, -(TR)
 *	2:				; write mode code generated by the
 *					; unify_ instructions goes here
 *
 *	Read mode:
 *	3:	subq	#2, D0		; see if we have a list pointer
 *		beq.s	1f		; branch if so
 *		jmp	fail
 *					; switch on term code might jump
 *					; into here and perform allocations
 *					; necessary for the determinate
 *					; case
 *					;
 *	1:	subq	#2, A1		; A1, the write mode pointer, is now
 *					; set up
 *
 */

void
ic_g_list(base,disp,x,y)
    long base;
    long disp;
    long x, y;
{
    int reg,mode;
    Code *l1,*l2;
    if (capturemode == WRITEMODE) {
	COMPUTE_MODE(base,disp,reg,mode)
	MOVE(reg,mode,disp,D0,DDIRECT,0)
	ic_deref1(1,&capturepatch1);		/* dereference instructions */
	LEA(H,DISPL,2,A0)
	MOVE(A0,ADIRECT,0,S,INDIRECT,0)	/* move.l A0, (A1)	*/
	CMPAD(S,HB)
	BLS(0)
	LABEL(l2)
	CMPAD(S,SPB)
	BHI(0)
	LABEL(l1)
#ifdef delay
	ic_delay_test(S);
#endif
	MOVE(S,ADIRECT,0,TR,PREDECR,0) /* move.l A1, -(A4)	*/
	PATCHDISP(l1)
	PATCHDISP(l2)
    }
    else {
	SUBQW(2,D0,DDIRECT,0)
	BEQ(0)
	LABEL(l1)
	DoFail
	if (!firstargprocessed)
	    ic_1stargalloc();
	PATCHDISP(l1)
	SUBQ(2,S,ADIRECT,0)
    }
}

/*
 * Instruction: ic_g_structure
 * Function:	emits code to unify a structure with an argument
 * Parameters:	funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, the the displacement is the number of the
 *	 temporary to use.
 * Code Summary:
 * 	Write mode:
 *		move.l	EA, D0
 *	1:	move.l	D0, A1
 *		and	#3, D0
 *		bne	3f
 *		move.l	(A1), D0
 *		cmp.l	A1, D0
 *		bne	1b
 *		lea	1(H), A0	; 1 is the structure tag
 *		move.l	A0, (A1)	; bind variable
 *		cmp.l	A1, HB
 *		bls.s	2f
 *		cmp.l	A1, SPB
 *		bhi.s	2f
 *		move.l	A1, -(TR)
 *	2:	move.l	#functor, (H)+
 *					; write mode code generated by the
 *					; unify_ instructions goes here
 *
 *	Read mode:
 *	3:	subq	#1, D0		; see if we have a structure pointer
 *		beq.s	1f		; branch if so
 *	2:	jmp	fail
 *					; switch on term code might jump
 *					; into here and perform allocations
 *					; necessary for the determinate
 *					; case
 *					;
 *	1:	subq	#1, A1		; A1, the write mode pointer, is now
 *					; set up
 *		cmp	#functor, (A1)+	; see if functors are the same
 *		bne	2b		; branch if not
 *
 */

void
ic_g_structure(funcid,arity,base,disp)
    long funcid, arity, base, disp;
{
    Code *l1,*l2;
    int reg,mode;
    int functor = MMK_FUNCTOR(funcid,arity);
    int ldisp;

    if (capturemode == WRITEMODE) {
	COMPUTE_MODE(base,disp,reg,mode)
	MOVE(reg,mode,disp,D0,DDIRECT,0)
	ic_deref1(1,&capturepatch1);		/* dereference instructions */
	LEA(H,DISPL,1,A0)
	MOVE(A0,ADIRECT,0,S,INDIRECT,0)	/* move.l A0, (A1)	*/
	CMPAD(S,HB)
	BLS(0)
	LABEL(l1)
	CMPAD(S,SPB)
	BHI(0)
	LABEL(l2)
#ifdef delay
	ic_delay_test(S);
#endif
	MOVE(S,ADIRECT,0,TR,PREDECR,0) /* move.l A1, -(A4)	*/
	PATCHDISP(l1)
	PATCHDISP(l2)
	MOVI(functor, H,POSTINCR,0)
    }
    else {
	SUBQW(1,D0,DDIRECT,0)
	BEQ(0)
	LABEL(l2)
	LABEL(l1)
	DoFail
	if (!firstargprocessed)
	    ic_1stargalloc();
	PATCHDISP(l2)
	SUBQ(1,S,ADIRECT,0)
	CMPI(functor,S,POSTINCR,0)
	ldisp = LDISP(l1);
	BNE(ldisp)
    }
}

/*
 * Instruction:	ic_u_sym
 * Function:	emits code for unify_symbol
 * Parameters:	sym	-- symbol to unify element of structure with
 *
 */

void
ic_u_sym(sym,x,y,z)
   long sym;
   long x, y, z;
{
    ic_u_const(MMK_SYM(sym),MTP_SYM);
}

/*
 * Instruction:	ic_u_int
 * Function:	emits code for unify_integer
 * Parameters:	i	-- integer to unify element of structure with
 *
 */

void
ic_u_int(i,x,y,z)
   long i;
   long x, y, z;
{
    ic_u_const(MMK_INT(i),MTP_INT);
}

/*
 * Instruction: ic_u_const
 * Description:	called by ic_u_sym and ic_u_int to emit the right code
 *		for each
 * Parameters:	constant	-- constant to unify with
 * Code Summary:
 *	Read Mode:
 *	
 *		move.l	S, A0
 *		addq	#4, S
 *	1:	move.l	(A0), D0
 *		cmp.l	A0, D0
 *		beq.s	1f
 *		move.l	D0, A0
 *		and	#3, D0
 *		beq.s	1b
 *		cmp	#constant, A0
 *		beq.s	2f
 *	For Integers,
 *		jmp	fail
 *	For Symbols,
 *		move.l	A0, D0
 *		addq.l	#1, D0
 *		and.w	#0xf, D0
 *		bne.s	3f
 *		move.l	A0, -(SP)
 *		move.l	#constant, -(SP)
 *		jsr	cmp_sym_uia
 *		addq.l	#8, SP
 *		test.l	D0,
 *		bne.s	2f
 *	3:	jmp	fail
 *	Both Integers and Symbols,
 *	1:	move.l	#constant, (A0)
 *		cmp.l	A0, HB
 *		bls.s	2f
 *		move	A0, -(TR)
 *	2:	
 *
 *	Write Mode:
 *
 *		move.l	#constant, (H)+
 */

static void
ic_u_const(constant, tp)
    long constant;
    long tp;
{
    Code *l1,*p1,*p2,*p3,*p4;
    int   ldisp;

    if (capturemode == READMODE) {
      	MOVE(S,ADIRECT,0,A0,ADIRECT,0)
      	ADDQ(4,S,ADIRECT,0)
      	LABEL(l1)		/* to jump back to */
      	MOVE(A0,INDIRECT,0,D0,DDIRECT,0)
      	CMPAD(A0,D0)
      	BEQ(0)
      	LABEL(p1)		/* to patch beq */
      	MOVE(D0,DDIRECT,0,A0,ADIRECT,0)
      	ic_put(0x0240);			/* and.w #3, D0   */
      	ic_put(3);			/*       ^^       */
      	ldisp = LDISP(l1);
      	BEQ(ldisp)
      	ic_put(0xb1fc);			/* cmpa.l #const, A0	*/
      	ic_putl(constant);		/*        ^^^^^^	*/
      	BEQ(0)
      	LABEL(p2)		/* to patch beq */
      	if (tp == MTP_SYM) {
	    MOVE(A0,ADIRECT,0,D0,DDIRECT,0)
	    ADDQ(1,D0,DDIRECT,0)
	    ANDIW(0xf,D0,DDIRECT,0)
	    BNE(0)
	    LABEL(p3)
	    MOVE(A0,ADIRECT,0,SP,PREDECR,0)
	    MOVI(constant,SP,PREDECR,0)
	    JSR((long) cmp_sym_uia)

	    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_cmp_sym_uia)
 
	    ADDQ(8,SP,ADIRECT,0)
	    TST(D0,DDIRECT,0)
	    BNE(0)
	    LABEL(p4)
	    PATCHDISP(p3)
      	}
      	else 
	    p4 = (Code *) 0;

     	DoFail
     	PATCHDISP(p1)
      	MOVI(constant,A0,INDIRECT,0)
      	CMPAD(A0,HB)
      	BLS(0)
      	LABEL(p1)
#ifdef delay
      	ic_delay_test(A0);
#endif
      	MOVE(A0,ADIRECT,0,TR,PREDECR,0)
      	PATCHDISP(p1)
      	PATCHDISP(p2)
      	if (p4) {
	    PATCHDISP(p4)
      	}
    }
    else {
	MOVI(constant,H,POSTINCR,0)
    }
}

/*
 * Instruction: ic_u_var
 * Function:	Implements the unify_variable instruction (for head matching)
 * Parameters:	base
 *		disp
 * Code Summary:
 *	Read mode:
 *		move.l	(S)+, EA
 *
 *	Write mode:
 *		move.l	H, EA
 *		move.l	H, (H)+
 */

void
ic_u_var(base,disp,argn,inbody)
    long base, disp, argn, inbody;
{
    int dreg, dmode;
    Code *l1;
    COMPUTE_MODE(base,disp,dreg,dmode)

    /*
    * The following code generates trailing code if the destination is an
    * environment cell.  This is necessary for GC to work properly.
    *
    * Note:  An optimization would be to generate the trailing code only
    *	in the body.
    */

    if (base == EREG && inbody) {
	LEA(dreg,dmode,disp,A0)
	dreg = A0;
	dmode = INDIRECT;
	disp = 0;
	CMPAD(A0,SPB)
	BHI(0)
	LABEL(l1)
	MOVE(A0,ADIRECT,0,TR,PREDECR,0) 	/* move.l A0, -(A4)	*/
	PATCHDISP(l1)
    }

    /*
    * Normal unify_variable stuff
    */

    if (capturemode == WRITEMODE) {
	MOVE(H,ADIRECT,0,dreg,dmode,disp)	/* move H, EA	*/
	MOVE(H,ADIRECT,0,H,POSTINCR,0)		/* move H, (H)+ */
    }
    else {
	MOVE(S,POSTINCR,0,dreg,dmode,disp)
    }
}

/*
 * Instruction: ic_u_val
 * Function:	Implements the unify_value instruction (for head matching)
 * Parameters:	base
 *		disp
 * Code Summary:
 *	Read mode:
 *		move.l	(S)+, D0
 *		move.l	EA, A0
 *		jsr	unify
 *	Write mode :
 *		move.l	EA, (H)+
 */

void
ic_u_val(base,disp,y,z)
    long base, disp;
    long y, z;
{
    int sreg, smode;
    COMPUTE_MODE(base,disp,sreg,smode);
    if (capturemode == READMODE) {
	MOVE(S,POSTINCR,0,0,DDIRECT,0)	/* move.l (S)+, D0	*/
	MOVE(sreg,smode,disp,0,ADIRECT,0)	/* move.l EA, A0	*/
#ifndef PACKAGE
	JSRX(UNIFY)
#else 	/* PACKAGE */
	JSR(UNIFY)
	RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_unify)
#endif 	/* PACKAGE */
    }
    else {
	MOVE(sreg,smode,disp,H,POSTINCR,0);
    }
}

/*
 * Instruction: ic_u_lval
 * Function:	Implements the unify_local_value instruction
 * Parameters:	base
 *		disp
 * Code Summary:
 *	Read mode:
 *		Same as ic_u_val read mode
 *	Write mode:
 *		move.l	SrcEA, D0	; set up D0 for dereferencing
 *
 *	1:	move.l	D0, A0		; standard dereference sequence
 *		and.w	#3, D0
 *		bne.s	2f
 *		move.l	(A0), D0
 *		cmp.l	A0, D0
 *		bne.s	1b
 *
 *		cmp.l	#wm_heapbase, D0 ; see if deref'd value is on stack
 *		bhs.s	2f		; branch if in heap
 *		move.l	H, (A0)		; bind to top of heap
 *		move.l	H, A0
 *		cmp	D0, SPB		; see if need to trail
 *		bhi.s	2f		; branch if we don't need to
 *		move	D0, -(TR)	; Trail it
 *	2:	move.l	A0, (H)+	; put object on heap
 */

void
ic_u_lval(base,disp,argn,x)
    long base, disp, argn;
    long x;
{
    if (capturemode == READMODE) {
	ic_u_val(base,disp,argn,x);
    }
    else {
	int reg,mode;
	COMPUTE_MODE(base,disp,reg,mode)
	MOVE(reg,mode,disp,D0,DDIRECT,0)
	JSR(wm_u_lval)
    }
}


/*
 * Instruction:	ic_u_void
 * Function:	emits instructions for handling variables in structure
 *		that occur only once in a clause
 * Parameters:	none
 * Code Summary:
 *	Read Mode:
 *		addq.l	#4, S
 *	Write Mode:
 *		move.l	H, (H)+
 */

void
ic_u_void(x,y,z,w)
    long x, y, z, w;
{
    if (capturemode == READMODE)
	ADDQ(4,S,ADIRECT,0)
    else
	MOVE(H,ADIRECT,0,H,POSTINCR,0)
}

/*
 * Instruction: ic_p_unsafe
 * Function:	emits a put_unsafe_value instruction
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary
 *		move.l	SrcEA, D0	; set up D0 for dereferencing
 *
 *	1:	move.l	D0, A0		; standard dereference sequence
 *		and.w	#3, D0
 *		bne.s	2f
 *		move.l	(A0), D0
 *		cmp.l	A0, D0
 *		bne.s	1b
 *
 *		cmp.l	#wm_heapbase, D0 ; see if deref'd value is on stack
 *		bhs.s	2f		; branch if in heap
 *		move.l	H, (A0)		; bind to top of heap
 *		move.l	H, A0
 *		move.l	H, (H)+		; create variable on heap
 *		cmp	D0, SPB		; see if need to trail
 *		bhi.s	2f		; branch if we don't need to
 *		move	D0, -(TR)	; Trail it
 *	2:	move.l	A0, DstEA	; store value
 *		
 *
 *
 * Note: if either sbase or dbase is TREG, then the displacement is the number
 *	of the temporary to use.
 * Note2: If the source dereferences to any stack variable, then this
 *	variable is bound to a heap variable.  This is somewhat different
 *	from wam implementations in where it is permissible to leave variables
 *	not in the environment alone.
 */

void
ic_p_unsafe(sbase,sdisp,dbase,ddisp)
    long sbase, sdisp, dbase, ddisp;
{
    long smode, sreg, dmode, dreg;

    COMPUTE_MODE(sbase,sdisp,sreg,smode)
    MOVE(sreg,smode,sdisp,D0,DDIRECT,0)
    JSR(wm_p_unsafe)
    COMPUTE_MODE(dbase,ddisp,dreg,dmode)
    MOVE(A0,ADIRECT,0,dreg,dmode,ddisp)
}


/*
 * Instruction ic_p_int
 * Function:	Moves the prolog representation of an integer to the
 *		destination.
 * Parameters:	i	-- integer
 *		base	-- destination base register
 *		disp	-- destination displacement
 * Code Summary:
 *		move.l	#int<i>, EA
 */

void
ic_p_int(i,base,disp,x)
    long i, base, disp;
    long x;
{
    ic_p_const(MMK_INT(i),base,disp);
}

/*
 * Instruction ic_p_sym
 * Function:	Moves the prolog representation of a symbol to the
 *		destination.
 * Parameters:	sym	-- symbol
 *		base	-- destination base register
 *		disp	-- destination displacement
 * Code Summary:
 *		move.l	#sym<i>, EA
 */

void
ic_p_sym(sym,base,disp,x)
    long sym, base, disp;
    long x;
{
    ic_p_const(MMK_SYM(sym),base,disp);
}

static void
ic_p_const(constant,base,disp)
    long constant,base,disp;
{
    int reg, mode;
    COMPUTE_MODE(base,disp,reg,mode)
    MOVI(constant,reg,mode,disp)
}

/*
 * Instruction:	ic_p_yvar
 * Function:	Installs an unbound variable at the given environment position
 *		and puts a reference to this variable in the destination.
 * Parameters:	ebase	-- base register needed to access environment
 *		edisp	-- displacement to access variable
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary:
 *	lea	edisp(ebase), A0
 *	move.l	A0, (A0)
 *	move.l	A0, EA
 *   where EA is the appropriate destination formed by dbase and ddisp
 * Note: if dbase is TREG then the displacement is the number of the
 *	 temporary to use.  ebase must not be TREG.
 */

void
ic_p_yvar(ebase,edisp,dbase,ddisp)
    long ebase, edisp, dbase, ddisp;
{
    int ereg, emode, dreg, dmode;

    COMPUTE_MODE(ebase,edisp,ereg,emode)
    LEA(ereg,emode,edisp,A0)
    MOVE(A0,ADIRECT,0,A0,INDIRECT,0)
    COMPUTE_MODE(dbase,ddisp,dreg,dmode)
    MOVE(A0,ADIRECT,0,dreg,dmode,ddisp)
}


/*
 * Instruction: ic_init_yvar1
 * Function:	Initializes an environment variable.  All environment variables
 *	        must be initialized prior to the first call for garbage
 *	        collection purposes.
 * Parameters:  ebase	-- base register needed to access environment
 *		edisp	-- displacement for accessing the variable
 *
 * Code Summary:
 *	lea	edisp(ebase), A0
 *	move.l	A0, (A0)+
 */

void
ic_init_yvar1(ebase,edisp,x,y)
    long ebase, edisp, x, y;
{
    int ereg, emode;
    COMPUTE_MODE(ebase,edisp,ereg,emode)
    LEA(ereg,emode,edisp,A0)
    MOVE(A0,ADIRECT,0,A0,POSTINCR,0)
}

/*
 * Instruction: ic_init_yvar2
 * Function:	Called after ic_init_yvar1 to efficiently initialize other
 *		environment variables.
 * Parameters:	incr	-- number when multiplied by 4 needed to add to A0
 *		for proper address.  Note that when the environment variables
 *		go sequentially, incr will be zero due to the use of the
 *		post increment on the move.
 *
 * Code Summary:
 *		adda.w	#incr*4, A0	(when incr > 2)
 *	or
 *		addq.l	#incr*4, A0	(when incr == 1 or incr == 2)
 *	or
 *		(nothing)		(when incr == 0)
 *
 *		move.l	A0, (A0)+
 */

void
ic_init_yvar2(incr,x,y,z)
    long incr;
    long x, y, z;
{
    incr *= 4;
    if (incr == 0)
	;
    else if (incr <= 8)
	ADDQ(incr, A0, ADIRECT, 0)
    else
	ADDIAW(incr, A0)
   
    MOVE(A0,ADIRECT,0, A0,POSTINCR,0)
}


/*
 * Instruction: ic_p_xvar
 * Function:	Installs an unbound variable on the top of the heap and
 *		puts a reference to this variable in the destination.
 *		The top of the heap is moved up by one location.
 * Parameters:	base	-- base register for destination
 *		disp	-- displacement to get to destination
 * Code Summary:
 *	move.l	H, EA
 *	move.l	H, (H)+
 * Note:	If base is TREG, then the displacement indicates the number
 *		of the temporary to use.
 */

void
ic_p_xvar(base,disp,x,y)
    long base, disp;
    long x, y;
{
    int reg, mode;
    COMPUTE_MODE(base,disp,reg,mode)
    MOVE(H,ADIRECT,0,reg,mode,disp)
    MOVE(H,ADIRECT,0,H,POSTINCR,0)
}

/*
 * Instruction: ic_p_list
 * Function:	emits code to put a pointer to a list structure in an argument
 * Parameters:	base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, then the displacement is the number of the 
 *	 temporary to use.
 * Code Summary:
 *	lea	2(H), A0
 *	move.l	A0, EA
 */

void
ic_p_list(base,disp,x,y)
   long base, disp;
   long x, y;
{
   int mode,reg;
   COMPUTE_MODE(base,disp,reg,mode)
   LEA(H,DISPL,2,A0)
   MOVE(A0,ADIRECT,0,reg,mode,disp)
}

/*
 * Instruction: ic_p_structure
 * Function:	emits code to put pointer to a structure in an argument
 * Parameters:	funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, the the displacement is the number of the
 *	 temporary to use.
 * Code Summary:
 *	lea	1(H), A0		; 1 is the structure tag
 *	move.l	A0, EA
 *	move.l	#functor, (H)+
 */

void
ic_p_structure(funcid,arity,base,disp)
    long funcid, arity, base, disp;
{
    int mode, reg;
    COMPUTE_MODE(base,disp,reg,mode)
    LEA(H,DISPL,1,A0)
    MOVE(A0,ADIRECT,0,reg,mode,disp)
    MOVI(MMK_FUNCTOR(funcid,arity),H,POSTINCR,0)
}

/*
 * Instruction: ic_endstruct
 * Function:	emits code to update the heap pointer after a sequence of
 *		unify instructions for put_structure or put_list.
 * Parameters:	None.
 */

void
ic_endstruct(x,y,z,w)
    long x, y, z, w;
{
    /* This is a nop on the SUN */
}

/*
 * Instruction: ic_do_cut
 * Function:	emits code for performing the cut operation
 * Parameters:	base	-- base register from which the environment is accessed
 *		disp	-- number of longwords to add to base to get to the
 *			   environment
 * Code Summary:
 *	If disp is zero:
 *		move.l	basereg, a0
 *		jsr	docut
 *	if disp is nonzero:
 *		lea	EA, a0
 *		jsr	docut
 */

void
ic_docut(base,disp,z,w)
    long base, disp;
    long z, w;
{
    if (disp == 0) {
      	MOVE(base,ADIRECT,0,A0,ADIRECT,0)
    }
    else {
      	LEA(base,DISPL,4*disp,A0)
    }
#ifndef PACKAGE
    JSRX(DOCUT)
#else 	/* PACKAGE */
    JSR(DOCUT)
    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_docut)
#endif 	/* PACKAGE */
}

/*
 * Instruction: ic_cut_proceed
 * Function:	Implements cut as the first, and only goal.
 * Parameters:	
 *		base	-- base from which to get the return address
 *		disp	-- displacement off of SP of return address
 *			   (in longwords)
 *		These displacements also work for loading A0 for the
 *		jump to the docut procedure.  rbase should be SP.
 * Code Summary:
 *	If rdisp is nonzero or rbase is not the stack pointer:
 *		lea	EA, SP
 *	The following two instructions are always emitted:
 *		move.l	SP, A0
 *		jmp	docut
 *
 */

void
ic_cut_proceed(base,disp,y,z)
    long base, disp;
    long y, z;
{
    int mode, reg;

    COMPUTE_MODE(base,disp,reg,mode)
    if (reg != SP || disp != 0)
    LEA(reg,mode,disp,SP)
    MOVE(SP,ADIRECT,0,A0,ADIRECT,0)
    MOVE(SP,POSTINCR,0,E,ADIRECT,0)
    JMP(DOCUT)

    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_docut)
}


/*
 * Instruction:	ic_deallocate_cut_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *		usually is emitted as a result of a cut as the last goal.
 * Parameters:	none
 * Code Summary:
 *	move.l	(E), A0
 *	move.l	-4(E), E
 *	move.l	A0, SP
 *	jmp	docut
 */

void
ic_deallocate_cut_proceed(w, x, y, z)
    long w, x, y, z;
{

    MOVE(E,ADIRECT,0,SP,ADIRECT,0)
    MOVE(E,ADIRECT,0,A0,ADIRECT,0)
    MOVE(SP,POSTINCR,0,E,ADIRECT,0)
    JMP(DOCUT)

    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_docut)
}

/*
 * Instruction:	ic_cutmacro
 * Function:	emits code needed for performing cuts within a cut macro
 * Parameters:	ebase	-- environment base register
 *		edisp	-- displacement to get to end of arguments
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary:
 *		
 *		move.l	#wm_heapbase, D0
 *		lea	(ebase,edisp*4), A0
 *		sub.l	A0, D0
 *		lsl.l	#4, D0
 *		addq.l	#3, D0
 *		move.l	D0, EA
 *
 * Note:  The idea here is to put a relatively small integer down which
 *	  will represent the cutpt.  The real cutpt may be found by loading
 *	  this location and shifting right by 4 bits.
 */

void
ic_cutmacro(ebase,edisp,dbase,ddisp)
    long ebase, edisp, dbase, ddisp;
{
    int mode, reg;
    COMPUTE_MODE(dbase,ddisp,reg,mode)
#ifndef PACKAGE
    MOVI((int) wm_heapbase, D0, DDIRECT, 0)
#else 	/* PACKAGE */
    MOVAbs((int) &wm_heapbase, D0, DDIRECT, 0)
    RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_heapbase)
#endif 	/* PACKAGE */
    LEA(ebase,DISPL,4*edisp,A0)
    SUBAD(A0,D0)
    LSLND(4,D0)
    ADDQ(MTP_INT,D0,DDIRECT,0)
    MOVE(D0,DDIRECT,0,reg,mode,ddisp)
}

#ifdef delay
void
ic_delay_test(reg, x, y, z)
    long reg;
    long x, y, z;
{
    Code *l;
    CMPI(MMK_FUNCTOR(TK_DELAY,4),reg,DISPL,-4)
    BNE(0)
    LABEL(l)
    MOVEQ(-2,OV)
    PATCHDISP(l)
}
#endif


void
ic_start_capture(x,y,z,w)
    long x, y, z, w;
{
    captureidx	= 0;
    capturemode	= CAPTUREMODE;
}

void
ic_begin_macro(x, y, z, w)
    long x, y, z, w;
{
    macroidx	= 0;
    capturemode	= MACROMODE;
}

void
ic_end_macro(keepit, x, y, z)
   long keepit;
   long x, y, z;
{
    capturemode	= WRITEMODE;
    if (keepit) {
	TST(OV,DDIRECT,0)
	BPL(0)
	LABEL(ic_macropatch1)
    }
}

void
ic_callinfo(mask,nargs,envsize,w)
    long mask;
    long nargs;
    long envsize;
    long w;
{
   callinfo[callidx].patchaddr = ic_ptr;
   callinfo[callidx].argenvsize = (envsize << 16) | nargs;
   callinfo[callidx].argmask = mask;

   callidx++;
   MOVIW(0,D0,DDIRECT,0)
}


#define ICODE(macro,str,addr,obp) {addr},

static struct {
    void  (*doit) PARAMS(( long, long, long, long ));
} instrs[] = {
#include "icodedef.h"
};

void
ic_put_macro(islast)
    long islast;
{
    int i,disp;

    if (!islast) {
	BRA(0)
      	ic_put(0);			/* always use word displacement */
    }

    disp = (ic_ptr-ic_macropatch1)*2;
    if (disp <= 127) {
      	*(ic_macropatch1-1) |= disp;		/* or in byte displacement */
    }
    else {	/* need to shift and make room for word displacement */
      	Code *ip;
      	for (ip=ic_ptr; ip >= ic_macropatch1; ip--)
	    *(ip+1) = *ip;

	RELOC_UPDATE(ic_macropatch1,sizeof(Code))

      	ic_ptr++;
      	*ic_macropatch1++ = disp+2;
    }
    ic_macropatch2 = ic_ptr-1;
    for (i=0; i<macroidx; i++) {
	(instrs[macroarea[i].iidx].doit)(macroarea[i].w,
					 macroarea[i].x,
					 macroarea[i].y,
					 macroarea[i].z);
	ICBUF_OVERFLOW_CHECK
    }
    if (!islast)
	*ic_macropatch2 = (ic_ptr-ic_macropatch2)*2;
}

void
ic_end_capture(x,y,z,w)
    long x, y, z, w;
{
    int i;
    int disp1, disp2,shift1,shift2;
    Code *ip;
    capturemode = WRITEMODE;
    for (i=0; i<captureidx; i++) {
        (instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					   capturearea[i].x,
					   capturearea[i].y,
					   capturearea[i].z);
	ICBUF_OVERFLOW_CHECK
    }
    BRA(0);
    capturepatch2 = ic_ptr;
    capturemode = READMODE;
    for (i=0; i<captureidx; i++) {
        (instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					   capturearea[i].x,
					   capturearea[i].y,
					   capturearea[i].z);
	ICBUF_OVERFLOW_CHECK
    }
    capturemode = WRITEMODE;

    /*
     * Time to fix up the patch addresses.  capturepatch1 is the index just
     * after the bne in the dereference code.  capturepatch2 is the index
     * of the word just after the bra instruction.  We would like to use
     * short addresses to patch both addresses, but this may not always be
     * possible.  Thus is may be necessary to do some code shifting in order to
     * put in word displacements.
     */
   
    disp2 = (ic_ptr-capturepatch2)*2;
    shift1 = shift2 = 0;
    if (disp2 >= 128) {
      	shift2++;
      	disp2 += 2;
    }
    disp1 = (capturepatch2-capturepatch1+shift2)*2;
    if (disp1 >= 128) {
      	shift1++;
      	shift2++;
      	disp1 += 2;
    }
    if (capturepatch2 <= firstargptr)
      	firstargptr += shift2;
    if (shift2) {
      	for (ip=ic_ptr; ip>=capturepatch2; ip--)
	    *(ip+shift2) = *ip;

	RELOC_UPDATE(capturepatch2,(shift2*sizeof(Code)))

	if (shift1) {
	    for (ip=capturepatch2-1; ip>=capturepatch1; ip--)
		*(ip+1) = *ip;

	    RELOC_UPDATE_FROM_TO(capturepatch2-1,capturepatch1,sizeof(Code))

	    *capturepatch1 = disp1;
	    *(capturepatch1+3) -= 2;
	    if (shift2 == 1)
		*capturepatch2 |= disp2;
	    else
		*(capturepatch2+1) = disp2;
	}
	else {
	    *(capturepatch1-1) |= disp1;
	    *capturepatch2 = disp2;
	}
	ic_ptr += shift2;
    }
    else {
      	*(capturepatch1-1) |= disp1;
      	*(capturepatch2-1) |= disp2;
    }
    capturepatch2 += shift2;
}

/*
 * ic_replacebranch is called after code terminating a clause has
 * been emitted.  ptr is a pointer to the beginning of this code.  If
 * the target of the branch instruction just before capturepatch2 is ptr,
 * then the branch instruction is replaced by the code between ptr and ic_ptr.
 *
 * If the branches are not presently short ones or if adding the code will
 * make them longer, the replacement is not done.
 */

static void
ic_replacebranch(ptr)
   	Code *ptr;
{
   	register int replen;
   	register Code *p,*q;
	
   	if (!capturepatch2)
      	return;
  	 
   	if (ptr-capturepatch2 > 62)
      	return;
   
   	if ( ((*(capturepatch2-1) & 0xff) >> 1) + capturepatch2 != ptr)
      	return;
   
   	replen = ic_ptr - ptr - 1;

   	if (capturepatch2-capturepatch1+replen > 62)
      	return;
   
   	for (p=ic_ptr; p>=capturepatch2; p--)
      	*(p+replen) = *p;
 
	RELOC_UPDATE(capturepatch2,(replen*sizeof(Code)))

   	ic_ptr += replen;
   	if (capturepatch2 <= firstargptr)
      	firstargptr += replen;
   
   	for (p=capturepatch2-1,q=ic_ptr-replen-1; q<ic_ptr; )
      	*p++ = *q++;
 
#ifdef PACKAGE
    if (rinfo_flag) {
        unsigned long *ri,*tori, *fromri;
        long patchloc, patchoff;
		int numofrobjs;
		/* 
		 * Find number of relocatable objects which will be copied 
		 */
        patchloc = (long)((long)(ic_ptr-replen-1) - (long)icode_buf);
        for (ri=rinfo_ptr - 1, numofrobjs=0;
             (ri >= rinfo_buf) && (RELOC_LOC(ri) >= patchloc); 
		     ri--, numofrobjs++) ;
		/* 
		 * Find first relocatable object after capturepatch2-1 
		 */
        patchloc = (long)((long)(capturepatch2-1) - (long)icode_buf);
        for (tori=rinfo_ptr - 1;
             (tori >= rinfo_buf) && (RELOC_LOC(tori) >= patchloc); tori--) ;
		tori++;
		/* 
		 * Shift all relocatable objects after tori 
		 */ 
        if ((((int)(rinfo_ptr-rinfo_buf))+numofrobjs) >= MAX_RINFO_ENTRIES)
            fprintf(stderr,"\nError: RInfo Buffer Full.");  
        for (ri=rinfo_ptr - 1; ri >= tori; ri--) {
            *(ri+numofrobjs) = *ri; 
		} 
		rinfo_ptr += numofrobjs;
		/* 
		 * Copy reloactable objects 
		 */
		fromri = rinfo_ptr - numofrobjs;
		patchoff = (long)((long)(ic_ptr-replen-1)-(long)(capturepatch2-1));
        for (ri=fromri; ri < rinfo_ptr; ri++, tori++) {
            *tori = ((*ri & 0xfffc0000) |
                     (RELOC_LOC(ri)-(long)(patchoff)));  
		} 

    }        
#endif  /* PACKAGE */
   
   	*(capturepatch1-1) += (replen*2);

}

static void
ic_1stargalloc()
{
    int oldmode = capturemode;
    int i;
    capturemode = READMODE;

    firstargptr = ic_ptr;
    for (i=0; i<allocidx; i++) {
        (instrs[allocarea[i].iidx].doit)(allocarea[i].w,
				         allocarea[i].x,
				         allocarea[i].y,
				         allocarea[i].z);

	ICBUF_OVERFLOW_CHECK

        if (allocarea[i].iidx == I_ALLOCATE1) {
	    i++;
	    while (allocarea[i].iidx != I_ENDALLOC1)
	       i++;
	    i--;
        }
    }
    firstargprocessed = 1;
    capturemode = oldmode;
}


void
icode(iidx,w,x,y,z)
    int  iidx;
    long x, y, z, w;
{
    static int proc_id, proc_arity;
    static int firstargkey;

#ifdef OBP
    if (makeobp)
        f_icode(iidx,w,x,y,z);
#endif
   
    if (iidx < 0) {
        switch (iidx) {
	    case IC_INIT :
		ic_ptr = icode_buf;
		callidx = 0;
		capturemode = WRITEMODE;
		firstargkey = MTP_UNBOUND;
		dstart = icode_buf;
		firstargptr = icode_buf;
		firstargprocessed = 0;
		capturepatch1 = (Code *) 0;
		capturepatch2 = (Code *) 0;
            
		/*
		 * Initialize clause relocation information
		 */
		RELOC_IC_INIT
      
	    break;
	    case IC_ENDCLAUSE :
		proc_id = w;
		proc_arity = x;
		if (firstargptr < dstart)
		    firstargptr = dstart;
		{   register int i;
		    ICBUF_OVERFLOW_CHECK1(callidx*(sizeof(long)/sizeof(Code)))
		    for (i=0; i<callidx; i++) {
		        if (*(callinfo[i].patchaddr+1)) {
			    /* macro patch has moved things on us */
			    *(callinfo[i].patchaddr+2) = 
					ic_ptr-callinfo[i].patchaddr-1;

		        }
		        else {
		   	    /* nothing has moved */
			    *(callinfo[i].patchaddr+1) =
					ic_ptr-callinfo[i].patchaddr;
		        }
		        ic_putl(callinfo[i].argenvsize);
		        ic_putl(callinfo[i].argmask);
		    }
		    ic_putl(-1);	/* -1 not allowed as a valid callinfo */
		    i = ic_ptr - icode_buf;	/* compute distance back to beginning */
		    ic_putl(i);		/* put it down */
		}
		break;
	    case IC_ASSERTA :
		w_asserta(proc_id,proc_arity,icode_buf,ic_ptr-icode_buf,
			  firstargkey,firstargptr-icode_buf,dstart-icode_buf,
			  0);
		break;
	    case IC_ASSERTZ :
		w_assertz(proc_id,proc_arity,icode_buf,ic_ptr-icode_buf,
			  firstargkey,firstargptr-icode_buf,dstart-icode_buf,
			  0);
		break;
	    case IC_ADDCLAUSE :
		w_addclause(proc_id,proc_arity,*top_clausegroup,
			    icode_buf,ic_ptr-icode_buf,
			    firstargkey,firstargptr-icode_buf,dstart-icode_buf,
			    0);
		break;
	    case IC_EXECQUERY :
		w_execquery(icode_buf,ic_ptr-icode_buf);
		break;
	    case IC_EXECCOMMAND :
		w_execcommand(icode_buf,ic_ptr-icode_buf);
		break;
	    case IC_ADDUSE :
		mod_adduse(cur_mod,w);
		break;
	    case IC_ENDMODULE :
		end_mod();
		break;
	    case IC_NEWMODULE :
		new_mod(w);		/* w is token id of new module */
		break;
	    case IC_EXPORTPRED :
		export_pred(cur_mod,w,x);
		break;
	    case IC_1STARG :
		switch (w) {
		    case TP_VO :
		        firstargkey = MTP_UNBOUND;
		        break;
		    case TP_LIST :
		        firstargkey = MTP_LIST;
		        break;
		    case TP_INT :
		        firstargkey = MMK_INT(z);
		        break;
		    case TP_SYM :
		        firstargkey = MMK_FUNCTOR(x,y);
		        break;
		}
		firstargprocessed = 1;
		break;
	    case IC_BEGINMACRO :
		ic_begin_macro(w, x, y, z);
		break;
	    case IC_ENDMACRO :
		ic_end_macro(w, x, y, z);
		ICBUF_OVERFLOW_CHECK
		break;
	    case IC_PUTMACRO :
		ic_put_macro(w);
		break;
	    case IC_CREMODCLOSURE :
		createModuleClosureProcedure(w,x,y);
		break;
	    case IC_ADDTO_AUTOUSE :
		add_default_use(w);
		break;
	    case IC_ADDTO_AUTONAME :
		add_default_proc((PWord) w,x);
		break;
	    case IC_BEGINALLOC :
		allocidx = 0;
		capturemode = ALLOCMODE;
		break;
	    case IC_ENDALLOC :
		capturemode = WRITEMODE;
		break;
	    case IC_ICRESET :
		break;
	    default :
		fprintf(stderr,"Warning: unrecognized icode command (%d).\n",
			iidx);
		break;
	}
    }
    else if (iidx == I_END_CAPTURE)
	ic_end_capture(w,x,y,z);
    else if (capturemode == CAPTUREMODE) {
        capturearea[captureidx].iidx = iidx;
        capturearea[captureidx].w    = w;
        capturearea[captureidx].x    = x;
        capturearea[captureidx].y    = y;
        capturearea[captureidx].z    = z;
        captureidx++;
    }
    else if (capturemode == MACROMODE) {
        macroarea[macroidx].iidx = iidx;
        macroarea[macroidx].w	 = w;
        macroarea[macroidx].x	 = x;
        macroarea[macroidx].y	 = y;
        macroarea[macroidx].z	 = z;
        macroidx++;
    }
    else if (capturemode == ALLOCMODE) {
        allocarea[allocidx].iidx = iidx;
        allocarea[allocidx].w	 = w;
        allocarea[allocidx].x	 = x;
        allocarea[allocidx].y	 = y;
        allocarea[allocidx].z	 = z;
        allocidx++;
        (instrs[iidx].doit)(w,x,y,z);
	ICBUF_OVERFLOW_CHECK
    }
    else {
        (instrs[iidx].doit)(w,x,y,z);
	ICBUF_OVERFLOW_CHECK
    }
}
