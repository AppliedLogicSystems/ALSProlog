/*
 * icode1.c		-- clause code generator for VAX
 *	Copyright (c) 1987-90 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/20/90
 * Revision History:
 *
 * Note:
 *	The 68K and 88K versions of icode1.c were used to help construct
 *	this file.
 */


#include <stdio.h>

#include "config.h"
#include "types.h"
#include "alloc.h"
#include "parser.h"
#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "mtypes.h"
#include "wintcode.h"
#include "winter.h"
#include "module.h"
#include "machinst.h"
#include "tokens.h"
#include "codegen.h"


#define READMODE 0		/* in read mode			*/
#define WRITEMODE 1		/* in write mode (the default)	*/
#define CAPTUREMODE 2		/* capture unify instrs for	*/
				/* read/write mode expansion	*/
#define MACROMODE 3		/* loading a macro expansion	*/
#define ALLOCMODE 4		/* emitting allocation code	*/

#define MAXCALLS 300
#define CAPTURESIZE 400
#define ICBUFSIZE 32768

int makeobp;

static struct capturestruct {
		int iidx;
		int w,x,y,z;
	      } capturearea[CAPTURESIZE];

static int captureidx;
static Code *capturepatch1;	/* patch indices for read/write mode */
static Code *capturepatch2;

static struct {			/* data structure for storing gc call info */
		Code *patchaddr;	
		long data;
	      } callinfo[MAXCALLS];
static int callidx;

static struct capturestruct macroarea[CAPTURESIZE];
static int macroidx;
Code *ic_macropatch1;
Code *ic_macropatch2;

static struct capturestruct allocarea[CAPTURESIZE];
static int allocidx;
static int firstargprocessed;	/* 1 if the first arg has been processed, 
				 * 0 otherwise.
				 */
static Code *dstart;		/* Pointer to the determinate case
				 * when we don't know anything about
				 * the first argument
				 */
static Code *firstargptr;	/*
				 * Pointer to the determinate code
				 * for the first argument in which
				 * the dereference code is skipped 
				 * because A1 has the dereference
				 * first argument.
				 */

static int capturemode = WRITEMODE;

Code icode_buf[ICBUFSIZE];
Code *ic_ptr;

/*
 * ic_deref is used to generate a dereference loop.  The readmode patch
 * 	address is returned as the value of this function.
 *
 *	reg is the register number to dereference.
 *
 *	r0 (TMP1) is used in this loop, so reg should not be set to this
 *	value.
 */

Code *
ic_deref(reg)
    int reg;
{
    LABEL deref, ground;

    BLABDCL(deref)			/* deref:			*/
	movl	REG(r0) REG(reg)	/*	movl 	r0, reg		*/
	bicb2	BIMM(0xfc) REG(r0)	/*	bicb 	#0xfc, r0	*/
	bneq	FLAB(ground)		/*	bneq	ground		*/
	movl	ATREG(reg) REG(r0)	/*	movl	(reg), r0	*/
	cmpl	REG(reg) REG(r0)	/*	cmpl	reg, r0		*/
	bneq	BLAB(deref)		/*	bneq	deref		*/

    return ground;			/* return address to patch later */
}


/*
 * Instruction: ic_addtosp
 * Function:	adds a number to the stack pointer
 * Parameters:	num	-- number to add
 */

ic_addtosp(num,x,y,z)
    int num;
    int x,y,z;				/* dummy parameters */
{

    if (num != 0) {
	num <<= 2;			/* multiply by four */
	if (0 < num && num < 64) {
	    /* Use an addl instruction with a short literal addressing mode */
	    addl2  SIMM(num) REG(SP)
	}
	else if (-64 < num && num < 0) {
	    /* Use a subl instruction with short literal */
	    subl2  SIMM(-num) REG(SP)
	}
	else {
	    /* Use an addl with the long immediate mode (a real space waster)*/
	    addl2  LIMM(num) REG(SP)
	}
    }
}


/*
 * Instruction:	ic_call
 * Function:	implements the procedure call
 * Parameters:	p	-- token index of procedure to call
 *		a	-- arity of the procedure
 */

ic_call(p,a,y,z)
    PWord p;
    int a;
    int y, z;				/* dummy parameters */
{
	jsb	ABSADDR(w_nameentry(cur_mod,p,a)->call_entry)
}


/*
 * Instruction:	ic_execute
 * Function:	implements the execute instruction (call of last goal)
 * Parameters:	p		-- token index of procedure to execute
 *		a		-- arity of the procedure
 */

ic_execute(p,a,x,y)
    PWord p;
    int a;
    int x, y;				/* dummy parameters */
{
	movl	REG(SP) REG(E)
	jmp	ABSADDR(w_nameentry(cur_mod,p,a)->exec_entry)
}


/*
 * Instruction:	ic_allocate
 * Function:	allocates space for first call in a multi-goal clause.
 * Parameters:	size	-- combined size of environment and arguments
 *				(in longwords)
 */

ic_allocate(size, x,y,z)
    int size;
    int x,y,z;				/* dummy parameters */
{
    ic_addtosp(-size,0,0,0);
}


/*
 * Instruction:	ic_allocate1
 * Function:	allocates space for goal in a clause with one goal
 * Parameters:	size	-- space on stack needed in non-determinate case
 *			   (this is usually equal to the size of the head)
 *
 */

ic_allocate1(size, x,y,z)
    int size;
    int x,y,z;				/* dummy parameters */
{
    if (capturemode != READMODE)
	ic_addtosp(-size,0,0,0);
}


/*
 * Instruction: ic_endallocate1
 * Function:	Records the dstart value
 */

ic_endallocate1(x,y,z,w)
    int x,y,z,w;			/* all dummy parameters */
{
    if (capturemode != READMODE) {
	dstart = ic_ptr;
    }
}

/*
 * Instruction: ic_deallocate
 * Function:	Sets up stack pointer for last call in a multi-goal clause.
 *		Also resores previous environment pointer.
 * Parameters:	size1	-- value in longwords to subtract from SPB giving
 *			   new stack position when things are non-determinate
 *		size2	-- value in longwords to subtract from E when the
 *			   clause is determinate.
 *		isdeterminateforsure
 *			-- 1 if the compiler know for certain that the
 *			   clause is determinate at this point; 0 otherwise.
 */

/*
 * Instruction: ic_deallocate1 through ic_deallocate4
 * Description: These four functions perform the same actions as ic_deallocate;
 *		the only difference is things are allowed to happen between
 *		the various phase for better efficiency.
 */

static LABEL deallocate2patch;
static LABEL deallocate3patch;

ic_deallocate1(w,x,y,z)
    int w,x,y,z;			/* all dummy parameters */
{
    deallocate3patch = (Code *) 0;
}

ic_deallocate2(size1, x,y,z)
    int size1;		/* size to use in non-determinate case */
    int x,y,z;
{
    size1 *= -4;	/* adjust for byte offset */
 
	cmpl	REG(E)	REG(SPB)
	blssu	FLAB(deallocate2patch)
	moval	DISP(SPB,size1) REG(SP)

}

ic_deallocate3(w,x,y,z)
    int w,x,y,z;			/* all dummy parameters */
{
    
	brb	FLAB(deallocate3patch)	/* generate branch around det case */
    FLABDCL(deallocate2patch)		/* backpatch */
}

ic_deallocate4(size2,x,y,z)
    int size2;		/* size to use in determinate case */
    int x,y,z;
{
    size2 *= -4;	/* adjust for byte offset */

    moval DISP(E,size2) REG(SP)
    
    if (deallocate3patch) {
	FLABDCL(deallocate3patch)	/* backpatch if used in nondet case */
    }
}


/*
 * Instruction:	ic_trim
 * Function:	Sets up stack pointer for next call in a multi-goal clause.
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
 *
 */

ic_trim(size1,size2,isdeterminateforsure, x)
    int size1, size2, isdeterminateforsure;
    int x;				/* dummy parameter */
{
    size1 *= -4;			/* adjust to get byte offsets */
    size2 *= -4;

    if (isdeterminateforsure) {
	int totsize = size1+size2;

	    moval	DISP(E,totsize) REG(SP)
    }
    else {
	LABEL around;

	    movl	REG(SPB) REG(r0)
	    cmpl	REG(E)   REG(SPB)
	    bgtru	FLAB(around)
	    moval	DISP(E,size1) REG(r0)
	FLABDCL(around)
	    moval	DISP(r0,size2) REG(SP)
    }
}


/*
 * Instruction:	ic_proceed
 * Function:	Implements the return from procedure
 * Parameters:	base	-- base from which to get the return address
 *		disp	-- displacement off of base to get return address
 *
 * Note:
 *	In doing the VAX implementation, I find that this code is somewhat
 *	buggy.  I believe that it always works properly because the compiler
 *	never attempts to pick up the return address and move it into a
 *	register in a clause in which a proceed will be generated.
 *
 *	Technically, we should have a cpbase and cpdisp and also an ebase
 *	and edisp.
 *
 *	The other implementations affected are the 68K and possibly I386.
 *	The 88K implementation uses registers for OldE and CP so there
 *	is no problem there.
 */

ic_proceed(base,disp, x,y)
    int base;
    int disp;
    int x,y;		/* dummy parameters */
{

	movl	LOC(base,disp) REG(r0)		/* get CP */
	movl	LOC(base,disp-1) REG(E)		/* get E */
	jmp	ATREG(r0)			/* jump to CP */
}


/*
 * Instruction: ic_inline_proceed
 * Function:	Deallocates an environment and then returns. This instruction
 *		is usually emitted as a result of a cut as the last goal.
 * Parameters:	none
 */

ic_inline_proceed(w,x,y,z)
    int w,x,y,z;	/* all dummy parameters */
{
	movl	BDISP(E,4) REG(r0)
	movl	ATREG(E) REG(E)
	jmp	ATREG(r0)
}

/*
 * Instruction:	ic_g_uia
 * Function:	Emits code for matching a uia in the head
 * Parameters:	uiastr		-- string corresponding to uia
 *		base		-- index of base register
 *		disp		-- displacement from base register
 */

ic_g_uia(uiastr,base,disp, x)
    char *uiastr;
    int base, disp;
    int x;		/* dummy parameter */
{
    extern wm_g_uia();

	movl	LOC(base,disp)	REG(r0)
	jsb	ABSADDR(wm_g_uia)
    ic_uiastr(uiastr);
}


/*
 * Instruction:	ic_p_uia
 * Function:	Emits code for putting down a uia in the body
 * Parameter:	uiastr		-- string corresponding to the UIA
 *		base		-- index of the base register
 *		disp		-- displacement from the base register
 */

ic_p_uia(uiastr, base, disp, x)
    char *uiastr;
    int base,disp;
    int x;		/* dummy parameter */
{
    extern wm_p_uia();

	jsb	ABSADDR(wm_p_uia)
    ic_uiastr(uiastr);
	movl	REG(r0) LOC(base,disp)
}

ic_uiastr(s)
    char *s;
{
    register int l,i;
    l = strlen(s)+1;
    if (l & 3)
	l = (l & ~3) + 4;		/* round up */

    l += 4;				/* add one longword for the fence */
    l >>= 2;				/* convert to longwords */
    ic_putw(l);				/* put down number of longwords-1 */
    ic_putl(MMK_FENCE(l));		/* put down the first fence	*/
    for (i=1; i<l; i++)
	ic_putl(*((long *) s)++);	/* put down the string 		*/
    ic_putl(MMK_FENCE(l));		/* put down another fence	*/
}

 
/*
 * Instruction ic_g_sym
 * Function:    Emits code corresponding to the symbol part of Warren's
 *              get_constant instruction
 * Parameters:  tokid   -- token index
 *              base    -- index of base register
 *              disp    -- displacement from base register
 */
 
ic_g_sym(tokid,base,disp,x)
    int tokid;
    int base;
    int disp;
    int x;			/* dummy parameter */
{
    extern wm_g_sym();

	movl	LOC(base,disp) REG(r1)
        move_const(MMK_SYM(tokid)); REG(r2)
	jsb	ABSADDR(wm_g_sym)

}
 
/*
 * Instruction: ic_g_int
 * Function:    Emits code corresponding to the integer part of Warren's
 *              get_constant instruction
 * Parameters:  i       -- integer to get
 *              base    -- index of base register
 *              disp    -- displacement from base register
 */
 
ic_g_int(i,base,disp,x)
{
    extern wm_g_int();

    	movl	LOC(base,disp) REG(r1)
        move_const(MMK_INT(i)); REG(r2)
    	jsb	ABSADDR(wm_g_int)

}

/*
 * move_const is used to move a constant into the destination which is
 * emitted by the the caller immediately after calling move_const.
 */

move_const(c)
    int c;
{
    if (0 <= c && c <= 63) {
	movl	SIMM(c)
    }
    else if (-63 <= c && c < 0) {
	mnegl	SIMM(-c)
    }
    else if (-128 <= c && c <= 127) {
	cvtbl	BIMM(c)
    }
    else if (-32768 <= c && c <= 32767) {
	cvtwl	WIMM(c)
    }
    else {
	movl	LIMM(c)
    }
}


/*
 * Instruction:	ic_move
 * Function:	emits a move instruction
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note: If either sbase or dbase is equal to REGS, the the displacement is
 *	 a register number to use.
 */

ic_move(sbase,sdisp,dbase,ddisp)
    int sbase, sdisp, dbase, ddisp;
{
	movl	LOC(sbase,sdisp)  LOC(dbase,ddisp)
}


/*
 * Instruction:	ic_g_value
 * Function:	emits code to unify two operands.
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note:	If either sbase or dbase is equal to REGS, then the
 *		displacement is the register number to use.
 */

extern wm_unify();

ic_g_value(sbase,sdisp,dbase,ddisp)
    int sbase, sdisp, dbase, ddisp;
{
    	movl	LOC(sbase,sdisp)  REG(r0)
	movl	LOC(dbase,ddisp)  REG(r1)
	jsb	ABSADDR(wm_unify)
}


/*
 * Instruction:	ic_g_list
 * Function:	emits code to unify an argument with a list
 * Parameters:	base	-- base register
 *		disp	-- displacement
 * Note:	If base is equal to REGS then the displacement is the
 *		register number to use.
 */

ic_g_list(base,disp, x,y)
    int base, disp;
    int x,y;		/* dummy parameters */
{
    LABEL l1, l2;

    if (capturemode == WRITEMODE) {
	movl	LOC(base,disp) REG(r0)
	capturepatch1 = ic_deref(S);

	/*
	 * We must bind the variable with a list pointer
	 */
	
	moval	BDISP(H,MTP_LIST) ATREG(S)
	cmpl	REG(S) REG(HB)
	bgequ	FLAB(l1)
	cmpl	REG(S) REG(SPB)
	blssu	FLAB(l2)
	movl	REG(S) AUTODECR(TR)

	FLABDCL(l1)
	FLABDCL(l2)
    }
    else {
	blbc	REG(r0) FLAB(l1)
	jmp	ATREG(FAIL)
	if (!firstargprocessed)
	    ic_1stargalloc();
	FLABDCL(l1)
	subl2	SIMM(2) REG(S)
    }
}


/*
 * Instruction:	ic_g_structure
 * Function:	emits code to unify a structure with an argument
 * Parameters:	funcid	-- token index of the functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 *
 * Note:	If base is TREG, then the displacement is the register 
 *		number to use.
 */

ic_g_structure(funcid,arity,base,disp)
    int funcid, arity;
    int base, disp;
{
    LABEL l1, l2;
    int functor = MMK_FUNCTOR(funcid,arity);

    if (capturemode == WRITEMODE) {
	movl	LOC(base,disp) REG(r0)
	capturepatch1 = ic_deref(S);

	moval	BDISP(H,MTP_STRUCT) ATREG(S)
	cmpl	REG(S) REG(HB)
	bgequ	FLAB(l1)
	cmpl	REG(S) REG(SPB)
	blssu	FLAB(l2)
	movl	REG(S) AUTODECR(TR)

	FLABDCL(l1)
	FLABDCL(l2)

	movl	LIMM(functor) AUTOINCR(H)

    }
    else {
	decb	REG(r0)
	beql	FLAB(l2)
	BLABDCL(l1)
	jmp	ATREG(FAIL)
	if (!firstargprocessed)
	    ic_1stargalloc();
	FLABDCL(l2)
	decl	REG(S)
	cmpl	LIMM(functor) AUTOINCR(S)
	bneq	BLAB(l1)
    }
}

/*
 * Instruction: ic_u_sym
 * Function:	emits code for unify_symbol
 * Parameters:	sym	-- symbol to unify element of structure with
 */

ic_u_sym(sym,x,y,z)
    int sym;
    int x,y,z;		/* dummy parameters */
{
    extern wm_u_sym();
    int c = MMK_SYM(sym);

    if (capturemode == WRITEMODE) {
	move_const(c); AUTOINCR(H)
    }
    else {
        move_const(c); REG(r2)
	jsb	ABSADDR(wm_u_sym)
    }
}


/*
 *
 * Instruction: ic_u_int
 * Function:	emits code for unify_integer
 * Parameter:	i -- integer to unify element of structure with
 */

ic_u_int(i, x,y,z)
    int i;
{
    extern wm_u_int();
    int c = MMK_INT(i);

    if (capturemode == WRITEMODE) {
	move_const(c); AUTOINCR(H)
    }
    else {
	move_const(c); REG(r2)
	jsb	ABSADDR(wm_u_int)
    }
}

/*
 * Instruction:	ic_u_var
 * Function:	Implements the unify_variable instruction (for head matching)
 * Parameters:	base
 *		disp
 */

ic_u_var(base, disp, argn, inbody)
    int base, disp, argn, inbody;
{
    LABEL l1;

    /*
     * The following code generates trailing code if the destination is an
     * environment cell.  This is necessary for GC to work properly.
     */
    
    if (base == EREG && inbody) {
	moval	LOC(base,disp) REG(r1)
	base = r1;
	disp = 0;
	cmpl	REG(r1) REG(SPB)
	blssu	FLAB(l1)
	movl	REG(r1) AUTODECR(TR)
	FLABDCL(l1)
    }

    /*
     * Normal unify_variable stuff
     */
    
    if (capturemode == WRITEMODE) {
	movl	REG(H) LOC(base,disp)
	movl	REG(H) AUTOINCR(H)
    }
    else {
	movl	AUTOINCR(S) LOC(base,disp)
    }
}


/*
 * Instruction:	ic_u_val
 * Function:	Implements the unify_value instruction (for head matching)
 * Parameters:	base		-- base register
 *		disp		-- displacement
 */

ic_u_val(base,disp, y, z)
    int base, disp;
    int y,z;			/* dummy parameters */
{
    if (capturemode == READMODE) {
	movl	AUTOINCR(S) REG(r0)
	movl	LOC(base,disp) REG(r1)
	jsb	ABSADDR(wm_unify)
    }
    else {
	movl	LOC(base,disp) AUTOINCR(H)
    }
}

/*
 * Instruction: ic_u_lval
 * Function:	Implements the unify_local_value instruction
 * Parameters:	base		-- base register
 *		disp		-- displacment
 */

ic_u_lval(base,disp,x,y)
    int base,disp;
    int x,y;
{
    extern wm_u_lval();
    if (capturemode == READMODE) {
	movl	AUTOINCR(S) REG(r0)
	movl	LOC(base,disp) REG(r1)
	jsb	ABSADDR(wm_unify)
    }
    else {
	movl	LOC(base,disp) REG(r0)
	jsb	ABSADDR(wm_u_lval)
    }
}


/*
 * Instruction: ic_u_void
 * Function:	emits instructions for handling variables in structure
 *		that occur only once in a clause
 * Parameters:	none
 */

ic_u_void(x,y,z,w)
    int x,y,z,w;		/* all dummy parameters */
{
    if (capturemode == READMODE) {
	addl2	SIMM(4) REG(S)
    }
    else {
	movl	REG(H) AUTOINCR(H)
    }
}

/*
 * Instruction:	ic_p_unsafe
 * Function:	emits a put_unsafe_vlaue instruction
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 */

ic_p_unsafe(sbase,sdisp,dbase,ddisp)
    int sbase,sdisp,dbase,ddisp;
{
    extern wm_p_unsafe();

    movl	LOC(sbase,sdisp) REG(r0)
    jsb		ABSADDR(wm_p_unsafe)
    movl	REG(r1) LOC(dbase,ddisp)
}

/*
 * Instruction: ic_p_int
 * Function:	Moves the prolog representation of an integer to the
 *		destination.
 * Parameters:	i	-- integer
 * 		base	-- destination base register
 *		disp	-- destination displacement
 */

ic_p_int(i,base,disp,x)
    int i, base,disp;
    int x;		/* dummy parameter */
{
    move_const(MMK_INT(i)); LOC(base,disp)
}

/*
 * Instruction: ic_p_sym
 * Function:	Moves the prolog representation of a symbol to the
 *		destination.
 * Parameters:	sym	-- symbol
 *		base	-- destination base register
 *		disp	-- destination displacement
 */

ic_p_sym(sym,base,disp,x)
    int sym, base, disp;
{
    move_const(MMK_SYM(sym)); LOC(base,disp)
}

/*
 * Instruction:	ic_p_yvar
 * Function:	Installs an unbound variable at the given environment postion
 *		and puts a reference to this variable in the destination.
 * Parameters:	ebase	-- base register needed to access environment
 *		edisp	-- displacement to access variable
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note:	If ebase or dbase is REGS, then the displacement is the number
 *		of the temporary to use.  The LOC pseudo-addressing mode knows
 *		about this distinction.
 */

ic_p_yvar(ebase,edisp,dbase,ddisp)
    int ebase,edisp,dbase,ddisp;
{
    edisp *= 4;
    moval	DISP(ebase,edisp) REG(r0)
    movl	REG(r0) ATREG(r0)
    movl	REG(r0) LOC(dbase,ddisp)
}


/*
 * Instruction: ic_init_yvar1
 * Function:	Intializes an environment variable. All environment variables
 *		must be initialized prior to the first call for garbage
 *		collection purposes.
 * Parameters:	ebase	-- base register needed to access environment
 *		edisp	-- displacement for accessing the variable
 *
 */

ic_init_yvar1(ebase,edisp,x,y)
    int ebase, edisp, x,y;
{
    edisp *= 4;
    moval	DISP(ebase,edisp) REG(r0)
    movl	REG(r0) AUTOINCR(r0)
}


/*
 * Instruction:	ic_init_yvar2
 * Function:	Called after ic_init_yvar1 to efficiently initialize other
 *		environment variables.
 * Parameters:	incr	-- number when multiplied by 4 needed to add to r0
 *		for proper address.  Note that when the environment variables
 *		go sequentially, incr will be zero due to the use of the 
 *		post increment on the move.
 */

ic_init_yvar2(incr,x,y,z)
    int incr;
{
    incr *= 4;
    if (incr == 0)
	;
    else if (incr < 64) {
	addl2	SIMM(incr) REG(r0)
    }
    else {
	addl2	LIMM(incr) REG(r0)
    }

    movl	REG(r0) AUTOINCR(r0)
}

/*
 * Instruction:	ic_p_xvar
 * Function:	Installs an unbound variable on the top of the heap and
 *		puts a reference to this variable in the destination.
 *		The top of the heap is moved up by one location.
 * Parameters:	base	-- base register for destination
 *		disp	-- displacement to get the destination
 * Note:	If base is REGS, then the displacement is the number of the
 *		temporary to use.  The LOC pseudo-addressing mode knows
 *		about this distinction.
 */

ic_p_xvar(base,disp,x,y)
    int base,disp;
    int x,y;
{
    movl	REG(H) LOC(base,disp)
    movl	REG(H) AUTOINCR(H)
}


/*
 * Instruction:	ic_p_list
 * Function:	emits code to put a pointer to the list structure in an
 *		argument
 * Parameters:	base	-- base register
 *		disp	-- displacement
 * Note:	If base is REGS, then the displacement is the number of the
 *		temporary to use.  The LOC pseudo-addressing mode knows
 *		about this distinction.
 */

ic_p_list(base,disp,x,y)
    int base, disp;
    int x,y;		/* dummy parameters */
{
    moval	BDISP(H,MTP_LIST) LOC(base,disp)
}

/*
 * Instruction:	ic_p_structure
 * Function:	emits code to put pointer to a structure in an argument
 * Parameters:	funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 * Note:	If base is REGS, then the displacement is the number of the
 *		temporary to use.  The LOC pseudo-addressing mode knows
 *		about this distinction.
 */

ic_p_structure(funcid,arity,base,disp)
    int funcid, arity;
    int base,disp;
{
    moval	BDISP(H,MTP_STRUCT) LOC(base,disp)
    movl	LIMM(MMK_FUNCTOR(funcid,arity)) AUTOINCR(H)
}

/*
 * Instruction:	ic_endstruct
 * Function:	emits code to update the heap pointer after a sequence of
 *		unify instructions for put_structure or put_list.
 * Parameters:	None.
 */

ic_endstruct(x,y,z,w)
    int w,x,y,z;	/* dummy parameters */
{
    /* This is a nop on the VAX */
}

/*
 * Instruction:	ic_do_cut
 * Function:	emits code for performing the cut operation
 * Parameters:	base	-- base register from which the environment is accessed
 *		disp	-- number of longwords to add to base to get to the
 *			   environment
 */

extern wm_docut();

ic_docut(base,disp,z,w)
    int base,disp;
    int z,w;		/* dummy parameters */
{
    disp *= 4;
    moval	DISP(base,disp) REG(r0)
    jsb		ABSADDR(wm_docut)
}

/*
 * Instruction:	ic_cut_proceed
 * Function:	Implements cut as the first, and only goal
 * Parameters:	base	-- base from which to get the return address
 *		disp	-- displacement of return address off of base
 *
 */

ic_cut_proceed(base,disp,y,z)
    int base, disp;
    int y,z;
{
    if (base != SP || disp != 0) {
	disp *= 4;
	moval	DISP(base,disp) REG(SP)
    }
    movl	REG(SP) REG(r0)
    movl	AUTOINCR(SP) REG(E)
    jmp		ABSADDR(wm_docut)
}

/*
 * Instruction:	ic_deallocate_cut_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *		usually is emitted as a result of a cut as the last goal.
 * Parameters:	None.
 */

ic_deallocate_cut_proceed(x,y,z,w)
    int x,y,z,w;
{
    movl	REG(E) REG(SP)
    movl	REG(E) REG(r0)
    movl	AUTOINCR(SP) REG(E)
    jmp		ABSADDR(wm_docut)
}

/* 
 * Instruction:	ic_cutmacro
 * Function:	emits code needed for performing cuts within a cut macro
 * Parameters:	ebase	-- environment base register
 *		edisp	-- displacement to get to end of arguments
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note:	The idea here is to put a relatively small integer down which
 *		will represent the cutpt.  The real cutpt may be found by
 *		loading this locations and shifting right by 4 bits.
 */

ic_cutmacro(ebase,edisp,dbase,ddisp)
    int ebase, edisp, dbase, ddisp;
{
    edisp *=4;

    moval	DISP(ebase,edisp) REG(r0)
    subl3	REG(r0) LIMM(wm_heapbase) REG(r0)
    ashl	SIMM(4) REG(r0) REG(r0)
    addl3	SIMM(MTP_INT) REG(r0) LOC(dbase,ddisp)

}

ic_callinfo(mask,nargs,z,w)
    int mask, nargs;
    int z,w;
{
    int dat;
    callinfo[callidx].patchaddr = ic_ptr;
    dat = (mask << 8) | nargs;
    if (dat == -1)
	dat &= 0x7fffffff;	/* -1 not allowed (it is the terminator) */
    
    callidx++;
    tstw	WIMM(0)		/* to be filled in later */
}

ic_start_capture(x,y,z,w)
{
    captureidx	= 0;
    capturemode	= CAPTUREMODE;
}

ic_begin_macro()
{
    macroidx	= 0;
    capturemode = MACROMODE;
}

ic_end_macro(keepit)
    int keepit;
{
    capturemode = WRITEMODE;
    if (keepit) {
	tstl	REG(SAFETY)
	bgeq	FLAB(ic_macropatch1)
    }
}


#define ICODE(macro,str,addr,obp) int addr();
#include "icodedef.h"
#undef ICODE

#define ICODE(macro,str,addr,obp) {addr},

static struct {
    int (*doit)();
} instrs[] = {
#include "icodedef.h"
};

ic_put_macro(islast)
    int islast;
{
    int i,disp;

    if (!islast) {
	brw	ic_putw(0);	/* Use word displacement */
	ic_macropatch2 = ic_ptr;
    }

    disp = ic_ptr-ic_macropatch1-1;
    if (disp <= 127) {
	*ic_macropatch1 = disp;
    }
    else {	/* need to shift and make room for brw instruction */
	Code *ip;
	for (ip=ic_ptr; ip > ic_macropatch1; ip--)
	    *(ip+3) = *ip;
	ic_ptr += 3;
	*(ic_macropatch1-1)	= iBLSS;	/* blss replaces bgeq */
	*ic_macropatch1		= 3;		/* branch around brw  */
	*(ic_macropatch1+1)	= iBRW;		/* put down the brw */
	*(short *)(ic_macropatch1+2) = disp;	/* put down the displacement */
	if (!islast) {				/* fix up the call info */
	    callinfo[callidx-1].patchaddr+=3;	/*  patch address	*/
	}
    }

    for (i=0; i<macroidx; i++) {
	(instrs[macroarea[i].iidx].doit)(macroarea[i].w,
					 macroarea[i].x,
					 macroarea[i].y,
					 macroarea[i].z);
    }

    if (!islast)
	*(short *)(ic_macropatch2-2) = (short) (ic_ptr-ic_macropatch2);
}

ic_end_capture(x,y,z,w)
    int x,y,z,w;		/* dummy parameters */
{
    int i;
    int disp1, disp2, shift1, shift2;
    Code *ip;

    capturemode = WRITEMODE;
    for (i=0; i<captureidx; i++) {
	(instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					   capturearea[i].x,
					   capturearea[i].y,
					   capturearea[i].z);
    }

    brb	FLAB(capturepatch2)

    capturemode = READMODE;

    for (i=0; i<captureidx; i++) {
	(instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					   capturearea[i].x,
					   capturearea[i].y,
					   capturearea[i].z);
    }

    capturemode = WRITEMODE;

    /*
     * Time to fix up the patch addresses.  capturepatch1 points to the
     * displacement to fix up in the bneq instruction in the dereference
     * code.  capturepatch2 points to the displacement of the brb instruction.
     * We would like to use byte displacments to patch both addresses, but
     * this may not always be possible.  Thus it may be necessary to do
     * some code shifting in order to put in word displacements.  On the
     * first one, we will actually need to place the bneq with a two
     * two instruction sequence to do the same thing.  The extra instruction
     * will take up 3 bytes.
     */
    
    disp2 = ic_ptr-capturepatch2-1;
    shift1 = shift2 = 0;
    if (disp2 >= 128) {
	*(capturepatch2-1) = iBRW;	/* replace brb with brw */
	shift2++;
    }
    disp1 = capturepatch2-capturepatch1+shift2;
    if (disp1 >= 128) {
	shift1 += 3;
	shift2 += 3;
    }
    if (capturepatch2 <= firstargptr)
	firstargptr += shift2;
    if (shift2) {
	for (ip=ic_ptr; ip > capturepatch2; ip--)
	    *(ip+shift2) = *ip;
	if (shift1) {
	    for (ip = capturepatch2; ip > capturepatch1; ip--)
		*(ip+shift1) = *ip;
	    *(capturepatch1-1)	= iBEQL;	/* replace bne instr */
	    *capturepatch1	= 3;		/* need to branch around brw */
	    *(capturepatch1+1)	= iBRW;		/* install brw instr */
	    *(short *)(capturepatch1+2) = disp1; /* install the displacement */
	    *(capturepatch1+11) -= 3;		/* fix up branch at bottom */
						/* 	of deref loop */
	    if (shift2 == 3) {			/* install byte displ */
		*(capturepatch2+shift1) = disp2;
	    }
	    else {	/* install word displacement */
		*(short *) (capturepatch2+shift1) = disp2; 
	    }

	}
	else {
	    *capturepatch1 = disp1;
	    *(short *) capturepatch2 = disp2;
	}
	ic_ptr += shift2;
    }
    else {
	*capturepatch1 = disp1;
	*capturepatch2 = disp2;
    }
    capturepatch2 += shift2;		/* ? */
}


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


icode(iidx,w,x,y,z)
{
   static int proc_id, proc_arity;
   static int firstargkey;

   if (makeobp)
      f_icode(iidx,w,x,y,z);
   
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
	    break;
	 case IC_ENDCLAUSE :
	    proc_id = w;
	    proc_arity = x;
	    if (firstargptr < dstart)
	       firstargptr = dstart;
            {  register int i;
	       for (i=0; i<callidx; i++) {
		  if (*(short *) (callinfo[i].patchaddr+2)) {
		     *(short *) (callinfo[i].patchaddr+5) =
					ic_ptr-(callinfo[i].patchaddr+3);
		  }
		  else {
		      *(short *) (callinfo[i].patchaddr+2) = 
					ic_ptr-callinfo[i].patchaddr;
		  }
		  ic_putl(callinfo[i].data);
	       }
	       ic_putl(-1);	/* -1 not allowed as a valid callinfo */
	       i = ic_ptr - icode_buf;	/* compute distance back to beginning */
	       ic_putl(i);		/* put it down */
	    }
	    break;
	 case IC_ASSERTA :
	    w_asserta(proc_id,proc_arity,icode_buf,ic_ptr-icode_buf,
		      firstargkey,firstargptr-icode_buf,dstart-icode_buf);
	    break;
	 case IC_ASSERTZ :
	    w_assertz(proc_id,proc_arity,icode_buf,ic_ptr-icode_buf,
		      firstargkey,firstargptr-icode_buf,dstart-icode_buf);
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
	    ic_begin_macro();
	    break;
	 case IC_ENDMACRO :
	    ic_end_macro(w);
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
	    fprintf(stderr,"Warning: unrecognized icode command (%d).\n",iidx);
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
      macroarea[macroidx].iidx	= iidx;
      macroarea[macroidx].w	= w;
      macroarea[macroidx].x	= x;
      macroarea[macroidx].y	= y;
      macroarea[macroidx].z	= z;
      macroidx++;
   }
   else if (capturemode == ALLOCMODE) {
      allocarea[allocidx].iidx	= iidx;
      allocarea[allocidx].w	= w;
      allocarea[allocidx].x	= x;
      allocarea[allocidx].y	= y;
      allocarea[allocidx].z	= z;
      allocidx++;
      (instrs[iidx].doit)(w,x,y,z);
   }
   else {
      (instrs[iidx].doit)(w,x,y,z);
   }
}
