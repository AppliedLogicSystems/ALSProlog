/*
 * icode1.c             -- stuff to emit instructions
 *      Copyright (c) 1987 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/12/87
 * Revision History:
 *      Revised: 03/22/87,      Kev             -- icode.c split into icode1.c,
 *                                                 icode2.c, and icode.h
 *      Revised: mm/dd/yy,      Who             -- Reason
 *
 */

#include "defs.h"
#include "wintcode.h"
#include "icode.h"
#include "icodegen.h"
#include "compile.h"
#include "module.h"
#include "machinst.h"
#include "codegen.h"
#include "rinfo.h"

#define ICODE(macro,str,addr,obp) void addr PARAMS(( long, long, long, long ));
#include "icodedef.h"
#undef ICODE

#define READMODE 0		/* in read mode                 */
#define WRITEMODE 1		/* in write mode (the default)  */
#define CAPTUREMODE 2		/* capturing unify instrs for   */
				/* read/write mode expansion    */
#define MACROMODE 3		/* loading a macro expansion    */
#define ALLOCMODE 4		/* emitting allocation code     */

#define MAXCALLS    300
#define CAPTURESIZE 400
#define ICBUFSIZE 196605

int   makeobp;

static struct capturestruct {
    long   iidx;
    long   w, x, y, z;
} capturearea[CAPTURESIZE];
static int captureidx;
static Code *capturepatch1;	/* patch indices for read/write mode */
static Code *capturepatch2;

static struct capturestruct macroarea[CAPTURESIZE];
static int macroidx;
Code *ic_macropatch1;

static struct capturestruct allocarea[CAPTURESIZE];
static int allocidx;
static int firstargprocessed;	/* 1 if the first argument has been
				 * processed.  0 otherwise.
				 */
static Code *dstart;		/*
				 * Pointer to the determinate case
				 * when we don't know anything about
				 * the first argument.
				 */

static Code *firstargptr;	/*
				 * Pointer to the determinate code
				 * for the first argument in which
				 * the dereference code is skipped
				 * because A1 has the dereferenced
				 * first argument.
				 */
Code *ic_failaddr;		/*
				 * place to jump to for failure.
				 * initially set to zero at the
				 * start of each clause.
				 */
static long envsavemask;	/*
				 * mask corresponding to top of
				 * stack of the locations to save
				 * when creating an environment.
				 *
				 * This value is also used to determine
				 * whether a clause requires the
				 * unit clause variations of the
				 * retry and trust code
				 */

static int capturemode = WRITEMODE;

Code  icode_buf[ICBUFSIZE];
Code *ic_ptr;

extern char *regnames[];

static	void	ic_uiastr	PARAMS(( char * ));
static	void	ic_1stargalloc	PARAMS(( void ));
static	Code *	ic_deref	PARAMS(( long, long, long, long ));
static	void	ic_begin_macro	PARAMS(( void ));
static	void	ic_end_macro	PARAMS(( long ));
static	void	ic_put_macro	PARAMS(( long ));

/*
 * ic_get_operand is given the base and displacement.  It will emit a load
 * instruction if necessary and return the register number the load is expected
 * to be in.
 */

long
ic_get_operand(base, disp, target)
    long   base, disp, target;
{
    if (base == 0)
	return disp;		/* disp is the register number */
    else {
	ldi(target, base, EBIAS + disp * 4);
	return target;
    }
}

/*
 * ic_put_operand is given a base, displacement, and source register.  It will
 * emit a store instruction if necessary.  If a register to register operation
 * is being performed, it will emit a move (an add to zero)
 */

void
ic_put_operand(base, disp, source)
    long   base, disp, source;
{
    if (base == 0) {
	if (disp != source) {
	    add(disp, source, ZERO_REG);
	}
    }
    else {
	sti(source, base, EBIAS + disp * 4);
    }
}


/*
 * ic_deref is used to generate a dereference loop.  The readmode patch address
 *      is returned as the value of the function.
 *
 * bi1 and bi2 (when non-zero) are instructions to be emitted early in
 * the dereference loop for the binding case.  These instructions will
 * execute while the load is taking place.  They may be executed more than
 * once.
 *
 * trtestreg (when non-zero) is the register to use for an early trail
 * test.  The condition codes are put into this register.
 *
 *
 */

static Code *
ic_deref(reg, trtestreg, bi1, bi2)
    long   reg;			/* number of address register to use in
				 * dereferencing
				 */
    long   trtestreg;
    long  bi1, bi2;		/* Instructions to put for the variable case */
{
    Code *top;
    long   tdisp;

    LABEL(top);
    bb1(BREF, reg, 0);		/* the zero will be patched later */
    ldi(TMP1_REG, reg, EBIAS);
    if (trtestreg)
	cmp(trtestreg, reg, HB_REG);
    if (bi1)
	ic_put(bi1);
    if (bi2)
	ic_put(bi2);
    cmp(reg, TMP1_REG, reg);
    COMP_DISP(tdisp, top);
    bb0n(EQ, reg, tdisp);
    add(reg, TMP1_REG, ZERO_REG);

    return top;
}


void
move_const(c, dst)
    unsigned long c;
    long   dst;
{
    if (hi16(c)) {
	oru(dst, ZERO_REG, hi16(c));
	ori(dst, dst, lo16(c));
    }
    else
	addui(dst, ZERO_REG, c);
}



/*
 * Instruction: ic_addtosp
 * Function:    adds a number to the stack pointer
 * Parameters:  num     -- number to add
 * Code summary:
 *      add.l   #num, SP
 */

void
ic_addtosp(num, x, y, z)
    long   num, x, y, z;
{
    num *= 4;

    if (num < 0)
	subui(SP_REG, SP_REG, -num);
    else if (num > 0)
	addui(SP_REG, SP_REG, num);
}


/*
 * Instruction: ic_call
 * Function:    implements the procedure call
 * Parameters:  p       -- token index of procedure to call
 *              a       -- arity of procedure
 * Code summary:
 *      call    p/a
 */

void
ic_call(p, a, y, z)
    long   p, a, y, z;
{
    long   disp;
    ntbl_entry *ent;

    ent = w_nameentry(cur_mod, p, a);
    disp = ent->call_entry - ic_ptr;
    bsrn(disp);
    RELOC_INFO(RELOC_PROC_CALL_OFF26, (ic_ptr - 1), 0)
	add(OldE_REG, E_REG, ZERO_REG);
}



/*
 * Instruction: ic_execute
 * Function:    implements the execute instruction (call of last goal)
 * Parameters:  p       -- token index of procedure to execute
 *              a       -- arity of procedure
 * Code summary:
 *      jmp     p/a
 */

void
ic_execute(p, a, x, y)
    long   p, a, x, y;
{
    long   disp;
    ntbl_entry *ent;

    ent = w_nameentry(cur_mod, p, a);
    disp = ent->exec_entry - ic_ptr;
    brn(disp);
    RELOC_INFO(RELOC_PROC_EXEC_OFF26, (ic_ptr - 1), 0)
	add(E_REG, SP_REG, ZERO_REG);
}


/*
 * Instruction: ic_allocate
 * Function:    allocates environment and space for first call in a multi-
 *              goal clause.
 * Parameters:  size    -- combined size of environment and arguments
 *                              (in longwords)
 * Code Summary:
 *
 */

void
ic_allocate(size, x, y, z)
    long   size;
    long   x, y, z;
{

    ic_addtosp(-size, 0, 0, 0);
}


/*
 * Instruction: ic_allocate1
 * Function:    allocates space for goal in a clause with one goal
 * Parameters:  size    -- space on stack needed in non-determinate case
 *                         (this is usually equal to the size of the
 *                          head)
 */


void
ic_allocate1(size, x, y, z)
    long   size;
    long   x, y, z;
{
    if (capturemode != READMODE)
	ic_addtosp(-size, 0, 0, 0);
}

/*
 * Instruction: ic_endallocate1
 * Function:    Puts down label (backpatches) for the ic_allocate1
 *              instruction.   See above.
 * Parameters:  none
 */

void
ic_endallocate1(x, y, z, w)
    long   x, y, z, w;
{
    if (capturemode != READMODE)
	dstart = ic_ptr;
}


/*
 * Instruction: ic_deallocate
 * Function:    Sets up stack pointer for last call in a multi-goal clause.
 * Parameters:  size1   -- value in longwords to subtract from AB giving
 *                         the new stack position when things are
 *                         non-determinate.
 *              size2   -- value in longwords to subtract from A when the
 *                         clause is determinate
 *              isdeterminateforsure
 *                      -- 1 if the compiler knows for certain that the clause
 *                         is determinate at this point; 0 otherwise.
 */


/*
 * Instruction: ic_deallocate1 through ic_deallocate4
 * Description: These four functions perform the same actions as ic_deallocate.
 */

static Code *deallocate2patch;
static Code *deallocate3patch;

void
ic_deallocate1(w, x, y, z)
    long   w, x, y, z;
{
    deallocate3patch = (Code *) 0;
}

void
ic_deallocate2(size1, x, y, z)
    long   size1;
    long   x, y, z;
{
    cmp(TMP1_REG, SPB_REG, E_REG);
    deallocate2patch = ic_ptr;
    bb1(HI, TMP1_REG, 0);
    subui(SP_REG, SPB_REG, size1 * 4);
}

void
ic_deallocate3(w, x, y, z)
    long   w, x, y, z;
{
    deallocate3patch = ic_ptr - 1;
    *ic_ptr = *(ic_ptr - 1);
    *(ic_ptr - 1) = BRN(0);
    ic_ptr++;
    PATCHDISP(deallocate2patch);
}

void
ic_deallocate4(size2, x, y, z)
    long   size2;
    long   x, y, z;
{
    subui(SP_REG, E_REG, size2 * 4);
    if (deallocate3patch)
	PATCHDISP(deallocate3patch);
}


/*
 * Instruction: ic_trim
 * Function:    Sets up stack pointer for next call in a multi-goal clause
 *              This code is not used before the first or last goals.  It
 *              also trims some of the environment away when possible.
 * Parameters:  size1   -- value in longwords of environment that must
 *                         persist after the call.
 *              size2   -- value in longwords of size of next goal or
 *                         difference in size of environment previously
 *                         and current size which ever is greater
 *              isdeterminateforsure
 *                      -- 1 if the compiler knows for certain that the
 *                         clause is determinate at this point; 0 otherwise
 * Code Summary:
 *      Determinate case:
 *              sub     SP, E, (size1+size2)*4
 *      Unknown case:
 *              cmp     S, SPB, E
 *              bb1.n   #lo, S, 1f
 *              add     T1, SPB, ZERO
 *              sub     T1, E, size1*4
 *      1:      sub     SP, T1, size2*4
 *
 */

void
ic_trim(size1, size2, isdeterminateforsure, x)
    long   size1, size2, isdeterminateforsure, x;
{
    long   realsize1 = 4 * size1;
    long   realsize2 = 4 * size2;

    if (isdeterminateforsure) {
	subui(SP_REG, E_REG, realsize1 + realsize2);
    }
    else {
	Code *p;

	/*
	 * The following code was added on 9/4/90 and should
	 * speed trim up by one clock.  
	 */

	cmp(S_REG, SPB_REG, E_REG);
	p = ic_ptr;
	bb1n(LO, S_REG, 0);
	subui(SP_REG, SPB_REG, realsize2);
	subui(SP_REG, E_REG, realsize1 + realsize2);
	PATCHDISP(p);
    }
}

/*
 * Instruction: ic_proceed
 * Function:    Implements the return from procedure
 * Parameters:  base    -- base from which to get the return address
 *              disp    -- displacement off of SP of return address
 *                         (in longwords)
 * Code Summary:
 *      jmp.n   CP
 *      add     E, OldE, ZERO
 *
 */

void
ic_proceed(base, disp, y, z)
    long   base, disp;
    long   y, z;
{
    jmpn(CP_REG);
    add(E_REG, OldE_REG, ZERO_REG);
}


/*
 * Instruction: ic_inline_proceed
 * Function:    Deallocates an environment and then returns.  This instruction
 *              usually is emitted as a result of an inline goal as the last
 * Parameters:  none
 * Code Summary:
 *      ld      CP, E, 32K+4
 *      br.n    CP
 *      ld      E, E, 32K
 */

void
ic_inline_proceed(w, x, y, z)
    long   w, x, y, z;
{
    ldi(CP_REG, E_REG, EBIAS + 4);
    jmpn(CP_REG);
    ldi(E_REG, E_REG, EBIAS + 0);
}

/*
 * Instruction: ic_g_uia
 * Function:    Emits code for matching a uia in the head
 * Parameters:  uiastr          -- string corresponding to uia
 *              base            -- index of base register
 *              disp            -- displacement from base register
 */

void
ic_g_uia(uiastr, base, disp, x)
    long  uiastr;
    long   base, disp;
    long   x;
{
    long   displ, reg;

    if (UARG1_REG != (reg = ic_get_operand(base, disp, UARG1_REG)))
	addu(UARG1_REG, reg, ZERO_REG);
    displ = ((Code *) wm_g_uia) - ic_ptr;
    bsr(displ);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_g_uia)
    ic_uiastr((char *) uiastr);
}

/*
 * Instruction: ic_p_uia
 * Function:    Emits code for putting down a uia in the body
 * Parameters:  uiastr          -- string corresponding to the UIA
 *              base            -- index of the base register
 *              disp            -- displacement from the base register
 *
 */

void
ic_p_uia(uiastr, base, disp, x)
    long  uiastr;
    long   base, disp;
    long   x;
{
    long   displ;

    displ = ((Code *) wm_p_uia) - ic_ptr;
    bsr(displ);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_p_uia)
    ic_uiastr((char *)uiastr);
    ic_put_operand(base, disp, UARG1_REG);
}

static void
ic_uiastr(s)
    char *s;
{
    register long l, i;

    l = strlen(s) + 1;
    if (l & 3)
	l = (l & ~3) + 4;	/* round up */

    l += 4;			/* add one longword for the fence */
    l >>= 2;			/* convert to longwords         */
    ic_put(MMK_FENCE(l));	/* put down the fence           */
    for (i = 1; i < l; i++)
	ic_put(*((long *) s)++);	/* put down the string          */

    /*
     * Make sure that there are all nulls following the last null in the last
     * word.
     */

    s = (char *) (ic_ptr - 1);
    while (*s++) ;
    *s++ = 0;
    *s++ = 0;
    *s++ = 0;

}


/*
 * Instruction ic_g_sym
 * Function:    Emits code corresponding to the symbol part of Warren's
 *              get_constant instruction
 * Parameters:  tokid   -- token index
 *              base    -- index of base register
 *              disp    -- displacement from base register
 */

void
ic_g_sym(tokid, base, disp, x)
    long   tokid, base, disp, x;
{
    long   displ, reg;

    if (UARG1_REG != (reg = ic_get_operand(base, disp, UARG1_REG))) {
	addui(UARG1_REG, reg, 0);
    }
    displ = ((Code *) wm_g_sym) - ic_ptr;
    bsrn(displ);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_g_sym)
	ori(UARG2_REG, ZERO_REG, tokid);
}

/*
 * Instruction : ic_g_dbl
 * Function:    Emits code for matching a double precision number
 * Parameters:  val1    -- low part of the double
 *              val2    -- high part of the double
 *              base    -- index of base register
 *              disp    -- displacement from base register
 */

void
ic_g_dbl(val1, val2, base, disp)
    long   val1, val2, base, disp;
{
    long   reg, displ;

    if (UARG1_REG != (reg = ic_get_operand(base, disp, UARG1_REG))) {
	addui(UARG1_REG, reg, 0);
    }
    ori(TMP1_REG, ZERO_REG, val1 & 0xffff);
    oru(TMP1_REG, TMP1_REG, (val1 >> 16) & 0xffff);
    ori(TMP2_REG, ZERO_REG, val2 & 0xffff);
    displ = ((Code *) wm_g_dbl) - ic_ptr;
    bsrn(displ);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_g_dbl)
	oru(TMP2_REG, TMP2_REG, (val2 >> 16) & 0xffff);
}

/*
 * Instruction: ic_p_dbl
 * Function:    Sets up an argument with a floating point number.
 * Parameters:  val1    -- least significant half of the double
 *              val2    -- most significant half of the double
 *              base    -- index of base register
 *              disp    -- displacement from base register
 *
 */

void
ic_p_dbl(val1, val2, base, disp)
    long   val1, val2, base, disp;
{
    long   reg;
    unsigned long f = MMK_FENCE(3);

    ori(TMP1_REG, ZERO_REG, f & 0xffff);
    oru(TMP1_REG, TMP1_REG, (f >> 16) & 0xffff);
    sti(TMP1_REG, H_REG, EBIAS);
    sti(TMP1_REG, H_REG, EBIAS + 12);
    ori(TMP1_REG, ZERO_REG, val1 & 0xffff);
    oru(TMP1_REG, TMP1_REG, (val1 >> 16) & 0xffff);
    sti(TMP1_REG, H_REG, EBIAS + 4);
    ori(TMP1_REG, ZERO_REG, val2 & 0xffff);
    oru(TMP1_REG, TMP1_REG, (val2 >> 16) & 0xffff);
    sti(TMP1_REG, H_REG, EBIAS + 8);

    if (base)
	reg = UARG1_REG;
    else
	reg = disp;

    oru(reg, H_REG, (MTP_DOUBLE >> 16) & 0xffff);
    ic_put_operand(base, disp, reg);
    addui(H_REG, H_REG, 16);

}


/*
 * Instruction: ic_g_int
 * Function:    Emits code corresponding to the integer part of Warren's
 *              get_constant instruction
 * Parameters:  i       -- integer to get
 *              base    -- index of base register
 *              disp    -- displacement from base register
 */

void
ic_g_int(i, base, disp, x)
    long   i;
    long   base;
    long   disp;
    long   x;
{
    Code *dp, *p1, *p2, *p3;
    long   reg, d;
    long   constant = MMK_INT(i);

    reg = ic_get_operand(base, disp, S_REG);
    ori(UARG1_REG, ZERO_REG, (constant) & 0xffff);
    oru(UARG1_REG, UARG1_REG, (((constant) >> 16) & 0xffff));

    dp = ic_deref(reg, UARG2_REG, 0, 0);
    p1 = ic_ptr;
    bb1n(HS, UARG2_REG, 0);
    sti(UARG1_REG, reg, EBIAS);
    cmp(UARG2_REG, reg, SPB_REG);
    p2 = ic_ptr;
    bb1(LO, UARG2_REG, 0);
    subui(TR_REG, TR_REG, 4);
    p3 = ic_ptr;
    brn(0);
    st(reg, TR_REG, ZERO_REG);

    PATCHDISP(dp);

    cmp(UARG2_REG, UARG1_REG, reg);
    if (ic_failaddr) {
	COMP_DISP(d, ic_failaddr);
	bb0(EQ, UARG2_REG, d);
    }
    else {
	bb1(EQ, UARG2_REG, 2);
	ic_failaddr = ic_ptr;
	jmp(FAIL_REG);
    }


    PATCHDISP(p1);
    PATCHDISP(p2);
    PATCHDISP(p3);

}



/*
 * Instruction: ic_move
 * Function:    emits a move instruction
 * Parameters:  sbase   -- source base register
 *              sdisp   -- source displacement
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 * Code Summary
 *              move.l  SourceEA,DestEA
 *
 * Note: if either sbase or dbase is TREG, then the displacement is the number
 *       of the temporary to use.
 */

void
ic_move(sbase, sdisp, dbase, ddisp)
    long   sbase, sdisp, dbase, ddisp;
{
    long   sreg;

    if (dbase)
	sreg = ic_get_operand(sbase, sdisp, TMP1_REG);
    else
	sreg = ic_get_operand(sbase, sdisp, ddisp);
    ic_put_operand(dbase, ddisp, sreg);
}


/*
 * Instruction: ic_g_value
 * Function:    emits code to unify the two operands.
 * Parameters:  sbase   -- source base register
 *              sdisp   -- source displacement
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 * Note: If either sbase or dbase is TREG, then the displacement is the number
 *       of the temporary to use.
 *
 * Code Summary:
 *              move    SEA, D0
 *              move    DEA, A0
 *              jsr     unify
 */

void
ic_g_value(sbase, sdisp, dbase, ddisp)
    long   sbase, sdisp, dbase, ddisp;
{
    long   udisp;

    ic_move(sbase, sdisp, REGS, UARG1_REG);
    ic_move(dbase, ddisp, REGS, UARG2_REG);
    udisp = ((Code *) wm_unify) - (ic_ptr - 1);
    *ic_ptr = *(ic_ptr - 1);
    *(ic_ptr - 1) = BSRN(udisp);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_unify)
	ic_ptr++;
}

/*
 * structure_offset is the offset from the beginning of a structure to
 *      access the next argument of a structure.  It is initialized by
 *      get_structure, get_list, put_structure, and put_list.  It is advanced
 *      by the unify_ instructions.
 */

static structure_offset;

/*
 * Instruction: ic_g_list
 * Function:    emits code to unify an argument with a list
 * Parameters:  base    -- base register
 *              disp    -- displacement
 *
 *
 */


void
ic_g_list(base, disp, x, y)
    long   base;
    long   disp;
    long   x, y;
{
    long   reg, d;
    Code *p1, *p2;

    if (capturemode == WRITEMODE) {
	reg = ic_get_operand(base, disp, S_REG);

	capturepatch1 = ic_deref(reg, UARG2_REG,
				 ORU(UARG1_REG, H_REG, LIST_MASK),
	/*
	 * CMP(TMP2_REG,reg,SPB_REG) );
	 */
				 0);

/* done in the deref loop
 * oru(UARG1_REG, H_REG, LIST_MASK);
 */
	p1 = ic_ptr;
	bb1n(HS, UARG2_REG, 0);
	sti(UARG1_REG, reg, EBIAS);
	cmp(TMP2_REG, reg, SPB_REG);
	p2 = ic_ptr;
	bb1(LO, TMP2_REG, 0);
	subui(TR_REG, TR_REG, 4);
	st(reg, TR_REG, ZERO_REG);

	PATCHDISP(p1);
	PATCHDISP(p2);

	structure_offset = 0;
    }
    else {
	if (base == 0)
	    reg = disp;
	else
	    reg = S_REG;

	if (!firstargprocessed) {
	    Code *p3 = ic_ptr;

	    bb1n(BLIST, reg, 0);
	    extui(S_REG, reg, 26, 0);
	    ic_failaddr = ic_ptr;
	    jmp(FAIL_REG);
	    ic_1stargalloc();
	    PATCHDISP(p3);
	}
	else if (ic_failaddr) {
	    COMP_DISP(d, ic_failaddr);
	    bb0(BLIST, reg, d);
	    extui(S_REG, reg, 26, 0);
	}
	else {
	    bb1n(BLIST, reg, 3);
	    extui(S_REG, reg, 26, 0);
	    ic_failaddr = ic_ptr;
	    jmp(FAIL_REG);
	}
	structure_offset = 0;
    }

}

/*
 * Instruction: ic_g_structure
 * Function:    emits code to unify a structure with an argument
 * Parameters:  funcid  -- token index of functor
 *              arity   -- arity of structure
 *              base    -- base register
 *              disp    -- displacement
 */

void
ic_g_structure(funcid, arity, base, disp)
    long   funcid;
    long   arity;
    long   base;
    long   disp;
{
    long   reg;
    Code *p1, *p2;
    long   d;
    long   functor = MMK_FUNCTOR(funcid, arity);


    if (capturemode == WRITEMODE) {
/* Done below in the dereference loop
 * ori(UARG1_REG,ZERO_REG,(functor)&0xffff);
 * oru(UARG1_REG,UARG1_REG,(((functor)>>16)&0xffff));
 */
	reg = ic_get_operand(base, disp, S_REG);

	capturepatch1 = ic_deref(reg, UARG2_REG,
			       ORI(UARG1_REG, ZERO_REG, (functor) & 0xffff),
		       ORU(UARG1_REG, UARG1_REG, ((functor >> 16) & 0xffff))
	    );

	sti(UARG1_REG, H_REG, EBIAS);	/* store the functor first */
	oru(UARG1_REG, H_REG, STRC_MASK);
	p1 = ic_ptr;
	bb1n(HS, UARG2_REG, 0);
	sti(UARG1_REG, reg, EBIAS);
	cmp(UARG2_REG, reg, SPB_REG);
	p2 = ic_ptr;
	bb1(LO, UARG2_REG, 0);
	subui(TR_REG, TR_REG, 4);
	st(reg, TR_REG, ZERO_REG);

	PATCHDISP(p1);
	PATCHDISP(p2);

	structure_offset = 4;
    }
    else {
	if (base == 0)
	    reg = disp;
	else
	    reg = S_REG;

	if (!firstargprocessed) {
	    Code *p3 = ic_ptr;

	    bb1n(BSTRC, reg, 0);
	    extui(S_REG, reg, 26, 0);
	    ic_failaddr = ic_ptr;
	    jmp(FAIL_REG);
	    ic_1stargalloc();
	    PATCHDISP(p3);
	}
	else if (ic_failaddr) {
	    COMP_DISP(d, ic_failaddr);
	    bb0(BSTRC, reg, d);
	    extui(S_REG, reg, 26, 0);
	}
	else {
	    bb1n(BSTRC, reg, 3);
	    extui(S_REG, reg, 26, 0);
	    ic_failaddr = ic_ptr;
	    jmp(FAIL_REG);
	}
	ldi(UARG2_REG, S_REG, EBIAS);
	ori(UARG1_REG, ZERO_REG, (functor) & 0xffff);
	oru(UARG1_REG, UARG1_REG, (((functor) >> 16) & 0xffff));
	cmp(UARG1_REG, UARG1_REG, UARG2_REG);
	COMP_DISP(d, ic_failaddr);
	bb0(EQ, UARG1_REG, d);

	structure_offset = 4;
    }

}

/*
 * Instruction: ic_u_sym
 * Function:    emits code for unify_symbol
 * Parameters:  sym     -- symbol to unify element of structure with
 *
 */

void
ic_u_sym(sym, x, y, z)
    long   sym;
    long   x, y, z;
{
    long   constant = MMK_SYM(sym);

    if (capturemode == WRITEMODE) {
	ori(UARG1_REG, ZERO_REG, (constant) & 0xffff);
	oru(UARG1_REG, UARG1_REG, (((constant) >> 16) & 0xffff));
	sti(UARG1_REG, H_REG, structure_offset + EBIAS);
    }
    else {
	long   d;

	addui(UARG1_REG, S_REG, structure_offset);
	d = ((Code *) wm_u_sym) - ic_ptr;
	bsrn(d);
	RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_u_sym)
	    ori(UARG2_REG, ZERO_REG, sym);
    }

    structure_offset += 4;
}


/*
 * Instruction: ic_u_int
 * Function:    emits code for unify_integer
 * Parameters:  i       -- integer to unify element of structure with
 *
 */

void
ic_u_int(i, x, y, z)
    long   i;
    long   x, y, z;
{
    long   constant = MMK_INT(i);

    if (capturemode == WRITEMODE) {
	ori(UARG1_REG, ZERO_REG, (constant) & 0xffff);
	oru(UARG1_REG, UARG1_REG, (((constant) >> 16) & 0xffff));
	sti(UARG1_REG, H_REG, structure_offset + EBIAS);
    }
    else {
	long   d;

	addi(UARG1_REG, S_REG, structure_offset);
	ori(UARG2_REG, ZERO_REG, (constant) & 0xffff);
	d = ((Code *) wm_u_int) - ic_ptr;
	bsrn(d);
	RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_u_int)
	    oru(UARG2_REG, UARG2_REG, (((constant) >> 16) & 0xffff));
    }
    structure_offset += 4;
}

/*
 * Instruction: ic_u_var
 * Function:    Implements the unify_variable instruction (for head matching)
 * Parameters:  base
 *              disp
 * Code Summary:
 *      Read mode:
 *              move.l  (S)+, EA
 *
 *      Write mode:
 *              move.l  H, EA
 *              move.l  H, (H)+
 */

void
ic_u_var(base, disp, argn, inbody)
    long   base, disp, argn, inbody;
{
    long   reg;
    Code *p1;


    if (base)
	reg = TMP1_REG;
    else
	reg = disp;

    if (capturemode == WRITEMODE) {
	/*
	 * The following code generates trailing code if the destination is an
	 * environment cell.  This is necessary for GC to work properly.
	 *
	 */

	if (base == E_REG && inbody) {
	    if (disp < 0)
		subui(TMP1_REG, E_REG, -disp * 4);
	    else
		addui(TMP1_REG, E_REG, disp * 4);
	    cmp(TMP2_REG, TMP1_REG, SPB_REG);
	    p1 = ic_ptr;
	    bb1(LO, TMP2_REG, 0);
	    subui(TR_REG, TR_REG, 4);
	    st(TMP1_REG, TR_REG, ZERO_REG);
	    PATCHDISP(p1);
	}

	addui(reg, H_REG, structure_offset);
	ic_put_operand(base, disp, reg);
	sti(reg, reg, EBIAS);
    }
    else {
	ldi(reg, S_REG, structure_offset + EBIAS);
	ic_put_operand(base, disp, reg);
    }
    structure_offset += 4;
}

/*
 * Instruction: ic_u_val
 * Function:    Implements the unify_value instruction (for head matching)
 * Parameters:  base
 *              disp
 * Code Summary:
 *      Read mode:
 *              move.l  (S)+, D0
 *              move.l  EA, A0
 *              jsr     unify
 *      Write mode :
 *              move.l  EA, (H)+
 */

void
ic_u_val(base, disp, y, z)
    long   base, disp;
    long   y, z;
{
    long   reg;
    long   udisp;

    if (capturemode == READMODE) {
	ldi(UARG1_REG, S_REG, structure_offset + EBIAS);
	reg = ic_get_operand(base, disp, UARG2_REG);
	if (reg != UARG2_REG)
	    add(UARG2_REG, reg, ZERO_REG);

	udisp = ((Code *) wm_unify) - (ic_ptr - 1);
	*ic_ptr = *(ic_ptr - 1);
	*(ic_ptr - 1) = BSRN(udisp);
	RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_unify)
	    ic_ptr++;
    }
    else {
	reg = ic_get_operand(base, disp, UARG1_REG);
	sti(reg, H_REG, structure_offset + EBIAS);
    }

    structure_offset += 4;
}

/*
 * Instruction: ic_u_lval
 * Function:    Implements the unify_local_value instruction
 * Parameters:  base
 *              disp
 */

void
ic_u_lval(base, disp, argn, x)
    long   base, disp, argn;
    long   x;
{

    if (capturemode == READMODE) {
	ic_u_val(base, disp, 0, 0);
    }
    else {
	long   reg, displ;

	if (UARG1_REG != (reg = ic_get_operand(base, disp, UARG1_REG)))
	    addui(UARG1_REG, reg, 0);
	displ = ((Code *) wm_u_lval) - ic_ptr;
	bsrn(displ);
	RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_u_lval)
	    addui(UARG2_REG, H_REG, structure_offset);

	structure_offset += 4;
    }
}

/*
 * Instruction: ic_u_void
 * Function:    emits instructions for handling variables in structure
 *              that occur only once in a clause
 * Parameters:  none
 * Code Summary:
 *      Read Mode:
 *              addq.l  #4, S
 *      Write Mode:
 *              move.l  H, (H)+
 */

void
ic_u_void(x, y, z, w)
    long   x, y, z, w;
{
    if (capturemode == READMODE) {
	structure_offset += 4;
    }
    else {
	addui(TMP1_REG, H_REG, structure_offset);
	sti(TMP1_REG, TMP1_REG, EBIAS);
	structure_offset += 4;
    }
}

/*
 * Instruction: ic_p_unsafe
 * Function:    emits a put_unsafe_value instruction
 * Parameters:  sbase   -- source base register
 *              sdisp   -- source displacement
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 * Code Summary
 *              move.l  SrcEA, D0       ; set up D0 for dereferencing
 *
 *      1:      move.l  D0, A0          ; standard dereference sequence
 *              and.w   #3, D0
 *              bne.s   2f
 *              move.l  (A0), D0
 *              cmp.l   A0, D0
 *              bne.s   1b
 *
 *              cmp.l   #wm_heapbase, D0 ; see if deref'd value is on stack
 *              bhs.s   2f              ; branch if in heap
 *              move.l  H, (A0)         ; bind to top of heap
 *              move.l  H, A0
 *              move.l  H, (H)+         ; create variable on heap
 *              cmp     D0, SPB         ; see if need to trail
 *              bhi.s   2f              ; branch if we don't need to
 *              move    D0, -(TR)       ; Trail it
 *      2:      move.l  A0, DstEA       ; store value
 *
 *
 *
 * Note: if either sbase or dbase is TREG, then the displacement is the number
 *      of the temporary to use.
 * Note2: If the source dereferences to any stack variable, then this
 *      variable is bound to a heap variable.  This is somewhat different
 *      from wam implementations in where it is permissible to leave variables
 *      not in the environment alone.
 */

void
ic_p_unsafe(sbase, sdisp, dbase, ddisp)
    long   sbase, sdisp, dbase, ddisp;
{
    long   displ, reg;

    if (UARG1_REG != (reg = ic_get_operand(sbase, sdisp, UARG1_REG))) {
	displ = ((Code *) wm_p_unsafe) - ic_ptr;
	bsrn(displ);
	RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_p_unsafe)
	    addui(UARG1_REG, reg, 0);
    }
    else {
	displ = ((Code *) wm_p_unsafe) - ic_ptr;
	bsr(displ);
	RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_p_unsafe)
    }
    ic_put_operand(dbase, ddisp, UARG1_REG);
}


/*
 * Instruction ic_p_int
 * Function:    Moves the prolog representation of an integer to the
 *              destination.
 * Parameters:  i       -- integer
 *              base    -- destination base register
 *              disp    -- destination displacement
 */

void
ic_p_int(i, base, disp, x)
    long   i, base, disp;
    long   x;
{
    long   reg;
    long   constant = MMK_INT(i);

    if (base)
	reg = UARG1_REG;
    else
	reg = disp;

    ori(reg, ZERO_REG, (constant) & 0xffff);
    oru(reg, reg, (((constant) >> 16) & 0xffff));

    ic_put_operand(base, disp, reg);
}

/*
 * Instruction ic_p_sym
 * Function:    Moves the prolog representation of a symbol to the
 *              destination.
 * Parameters:  sym     -- symbol
 *              base    -- destination base register
 *              disp    -- destination displacement
 * Code Summary:
 *              move.l  #sym<i>, EA
 */

void
ic_p_sym(sym, base, disp, x)
    long   sym, base, disp;
    long   x;
{
    long   reg;
    long   constant = MMK_SYM(sym);

    if (base)
	reg = UARG1_REG;
    else
	reg = disp;

    ori(reg, ZERO_REG, (constant) & 0xffff);
    oru(reg, reg, (((constant) >> 16) & 0xffff));

    ic_put_operand(base, disp, reg);
}


/*
 * Instruction: ic_p_yvar
 * Function:    Installs an unbound variable at the given environment position
 *              and puts a reference to this variable in the destination.
 * Parameters:  ebase   -- base register needed to access environment
 *              edisp   -- displacement to access variable
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 * Code Summary:
 *      lea     edisp(ebase), A0
 *      move.l  A0, (A0)
 *      move.l  A0, EA
 *   where EA is the appropriate destination formed by dbase and ddisp
 * Note: if dbase is TREG then the displacement is the number of the
 *       temporary to use.  ebase must not be TREG.
 */

void
ic_p_yvar(ebase, edisp, dbase, ddisp)
    long   ebase, edisp, dbase, ddisp;
{
    long   ereg;

    if (dbase)
	ereg = UARG1_REG;
    else
	ereg = ddisp;

    if (edisp < 0)
	subui(ereg, ebase, -edisp * 4);
    else
	addui(ereg, ebase, edisp * 4);
    sti(ereg, ereg, EBIAS);

    ic_put_operand(dbase, ddisp, ereg);
}


/*
 * Instruction: ic_init_yvar1
 * Function:    Initializes an environment variable.  All environment variables
 *              must be initialized prior to the first call for garbage
 *              collection purposes.
 * Parameters:  ebase   -- base register needed to access environment
 *              edisp   -- displacement for accessing the variable
 *
 * Code Summary:
 *      lea     edisp(ebase), A0
 *      move.l  A0, (A0)+
 */

void
ic_init_yvar1(ebase, edisp, x, y)
    long   ebase, edisp, x, y;
{
    if (edisp < 0)
	subui(UARG1_REG, ebase, -edisp * 4);
    else
	addui(UARG1_REG, ebase, edisp * 4);
    sti(UARG1_REG, UARG1_REG, EBIAS);
}

/*
 * Instruction: ic_init_yvar2
 * Function:    Called after ic_init_yvar1 to efficiently initialize other
 *              environment variables.
 * Parameters:  incr    -- number when multiplied by 4 needed to add to A0
 *              for proper address.  Note that when the environment variables
 *              go sequentially, incr will be zero due to the use of the
 *              post increment on the move.
 *
 * Code Summary:
 *              adda.w  #incr*4, A0     (when incr > 2)
 *      or
 *              addq.l  #incr*4, A0     (when incr == 1 or incr == 2)
 *      or
 *              (nothing)               (when incr == 0)
 *
 *              move.l  A0, (A0)+
 */

void
ic_init_yvar2(incr, x, y, z)
    long   incr;
    long   x, y, z;
{
    addui(UARG1_REG, UARG1_REG, (incr + 1) * 4);
    sti(UARG1_REG, UARG1_REG, EBIAS);
}


/*
 * Instruction: ic_p_xvar
 * Function:    Installs an unbound variable on the top of the heap and
 *              puts a reference to this variable in the destination.
 *              The top of the heap is moved up by one location.
 * Parameters:  base    -- base register for destination
 *              disp    -- displacement to get to destination
 * Code Summary:
 *      move.l  H, EA
 *      move.l  H, (H)+
 * Note:        If base is TREG, then the displacement indicates the number
 *              of the temporary to use.
 */

void
ic_p_xvar(base, disp, x, y)
    long   base, disp;
    long   x, y;
{
    long   reg;

    if (base)
	reg = UARG1_REG;
    else
	reg = disp;
    add(reg, H_REG, ZERO_REG);
    addui(H_REG, H_REG, 4);
    sti(reg, reg, EBIAS);
    ic_put_operand(base, disp, reg);
}

/*
 * Instruction: ic_p_list
 * Function:    emits code to put a pointer to a list structure in an argument
 * Parameters:  base    -- base register
 *              disp    -- displacement
 */

void
ic_p_list(base, disp, x, y)
    long   base;
    long   disp;
    long   x, y;
{
    long   reg;

    if (base)
	reg = UARG1_REG;
    else
	reg = disp;

    structure_offset = 0;

    oru(reg, H_REG, LIST_MASK);

    ic_put_operand(base, disp, reg);
}

/*
 * Instruction: ic_p_structure
 * Function:    emits code to put pointer to a structure in an argument
 * Parameters:  funcid  -- token index of functor
 *              arity   -- arity of structure
 *              base    -- base register
 *              disp    -- displacement
 */

void
ic_p_structure(funcid, arity, base, disp)
    long   funcid, arity, base, disp;
{
    long   reg;
    long   functor = MMK_FUNCTOR(funcid, arity);

    if (base)
	reg = UARG1_REG;
    else
	reg = disp;

    ori(UARG1_REG, ZERO_REG, (functor) & 0xffff);
    oru(UARG1_REG, UARG1_REG, (((functor) >> 16) & 0xffff));
    sti(UARG1_REG, H_REG, EBIAS);
    structure_offset = 4;
    oru(reg, H_REG, STRC_MASK);

    ic_put_operand(base, disp, reg);
}

/*
 * Instruction: ic_endstruct
 * Function:    emits code to update the heap pointer after a sequence of
 *              unify instructions for put_structure or put_list.
 * Parameters:  None.
 */

void
ic_endstruct(x, y, z, w)
    long   x, y, z, w;
{
    addui(H_REG, H_REG, structure_offset);
}


/*
 * Instruction: ic_do_cut
 * Function:    emits code for performing the cut operation
 * Parameters:  base    -- base register from which the environment is accessed
 *              disp    -- number of longwords to add to base to get to the
 *                         environment
 * Code Summary:
 *      If disp is zero:
 *              move.l  basereg, a0
 *              jsr     docut
 *      if disp is nonzero:
 *              lea     EA, a0
 *              jsr     docut
 */

void
ic_docut(base, disp, z, w)
    long   base;
    long   disp;
    long   z, w;
{
    long   d;

    COMP_DISP(d, ((Code *) wm_docut));
    bsrn(d);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_docut)
	addui(UARG1_REG, base, 4 * disp);
}

/*
 * Instruction: ic_cut_proceed
 * Function:    Implements cut as the first, and only goal.
 * Parameters:
 *              base    -- base from which to get the return address
 *              disp    -- displacement off of SP of return address
 *                         (in longwords)
 *              These displacements also work for loading A0 for the
 *              jump to the docut procedure.  rbase should be SP_REG.
 *
 */

void
ic_cut_proceed(base, disp, y, z)
    long   base, disp, y, z;
{
    long   d;

    addui(UARG1_REG, base, 4 * disp);
    add(E_REG, OldE_REG, ZERO_REG);
    COMP_DISP(d, ((Code *) wm_docut));
    brn(d);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_docut)
	add(RET_REG, CP_REG, ZERO_REG);
}


/*
 * Instruction: ic_deallocate_cut_proceed
 * Function:    Deallocates an environment and then returns.  This instruction
 *              usually is emitted as a result of a cut as the last goal.
 * Parameters:  none
 * Code Summary:
 *      move.l  (E), A0
 *      move.l  -4(E), E
 *      move.l  A0, SP
 *      jmp     docut
 */

void
ic_deallocate_cut_proceed(x, y, z, w)
    long   x, y, z, w;
{
    long   disp;

    addui(UARG1_REG, E_REG, 0);
    ldi(E_REG, E_REG, EBIAS);

    /*
     * The ldi and the add look stupid being done this way, but I think
     * it is necessary.
     */

    ldi(CP_REG, UARG1_REG, EBIAS + 4);
    COMP_DISP(disp, ((Code *) wm_docut));
    brn(disp);
    RELOC_INFO(RELOC_GVAR_OFF26, (ic_ptr - 1), symidx_wm_docut)
	add(RET_REG, CP_REG, ZERO_REG);
}

/*
 * Instruction: ic_cutmacro
 * Function:    emits code needed for performing cuts within a cut macro
 * Parameters:  ebase   -- environment base register
 *              edisp   -- displacement to get to end of arguments
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 */

void
ic_cutmacro(ebase, edisp, dbase, ddisp)
    long   ebase, edisp, dbase, ddisp;
{
    long   reg;

    if (dbase)
	reg = UARG1_REG;
    else
	reg = ddisp;
    if (edisp < 0)
	subui(reg, ebase, -edisp * 4);
    else
	addui(reg, ebase, edisp * 4);
    oru(reg, reg, INT_MASK);
    ic_put_operand(dbase, ddisp, reg);
}

void
ic_start_capture(x, y, z, w)
    long   x, y, z, w;
{
    captureidx = 0;
    capturemode = CAPTUREMODE;
}

static void
ic_begin_macro()
{
    macroidx = 0;
    capturemode = MACROMODE;
}

static void
ic_end_macro(keepit)
    long   keepit;
{
    capturemode = WRITEMODE;
    if (keepit) {
	ic_macropatch1 = ic_ptr;
	bcnd(GT0, SAFETY_REG, 0);
    }
}

/*
 * Instruction: ic_callinfo
 * Function:    Installs the garbage collector call information.  This call
 *              information consists of executable instructions from which
 *              the number of arguments to the procedure and the argument
 *              mask may be extracted.  The instructions are not meant to
 *              be executed, but if they are, nothing untoward will happen
 *              as they are nops.
 *
 *              The following paragraph is out of date:
 *
 *              In the 88k implementation, the garbage collector information
 *              consists of two addu instructions with r0 as the destination
 *              register.  The unsigned sixteen bit constant in the first
 *              instruction is the register mask.  The unsigned sixteen bit
 *              constant in the second instruction is the number of words
 *              back to the beginning of the clause from the instruction itself.
 *              This information is used by the code space garbage collector.
 *              Finally, the number of arguments (10 bits worth) is split
 *              between the source registers of the two instructions.  The
 *              source register of the first addu represents the low 5 bits
 *              of the number of arguments while the second represents the
 *              upper five bits.
 *
 *              The following paragraph is up to date:
 *
 *              In the 88k implementation, the garbage collector information
 *              consists of an addu instruction as the first word with r0
 *              as the destination register.   The low sixteen bits (we
 *              could use as many as 21 if necessary) will contain an offset
 *              back to the start of the clause from the addu instruction.
 *              The following two words unlike the earlier format are pure
 *              data:  The first of these two words consists of size information
 *              with the low sixteen bits representing the number of arguments
 *              and the high sixteen bits representing the size of the
 *              environment.  Finally, the last (32 bit) word is the argument
 *              mask.
 *
 * Parameters:  msk             -- argument usage mask
 *              nargs           -- number of arguments
 *              envsize         -- number of environment variables
 */

void
ic_callinfo(msk, nargs, envsize, w)
    long   msk;
    long   nargs;
    long   envsize;
    long   w;
{
    long   dat;


    if (!envsavemask) {
	envsavemask = ((msk << 2) | 0x3) & 0x1f;
    }

    dat = ic_ptr - icode_buf;
    addui(ZERO_REG, 0, dat);
    ic_put((envsize << 16) | nargs);
    ic_put(msk);
}


#define ICODE(macro,str,addr,obp) {addr},

static struct {
    void  (*doit) PARAMS(( long, long, long, long ));
} instrs[] =
{
#include "icodedef.h"
};

static void
ic_put_macro(islast)
    long   islast;
{
    long   i;
    Code *p1 = NULL;		/* stifle -Wall */

    if (!islast) {
	p1 = ic_ptr;
	br(0);
    }
    PATCHDISP(ic_macropatch1);
    ic_macropatch1++;		/* now it points just past the bcnd instr */
    for (i = 0; i < macroidx; i++) {
	(instrs[macroarea[i].iidx].doit) (macroarea[i].w,
					  macroarea[i].x,
					  macroarea[i].y,
					  macroarea[i].z);
    }
    if (!islast)
	PATCHDISP(p1);

}

void
ic_end_capture(x, y, z, w)
    long   x, y, z, w;
{
    long   i;

    capturemode = WRITEMODE;
    for (i = 0; i < captureidx; i++) {
	(instrs[capturearea[i].iidx].doit) (capturearea[i].w,
					    capturearea[i].x,
					    capturearea[i].y,
					    capturearea[i].z);
    }

    capturepatch2 = ic_ptr;
    brn(0);
    addui(H_REG, H_REG, structure_offset);
    *capturepatch1 |= (ic_ptr - capturepatch1);
    capturemode = READMODE;
    for (i = 0; i < captureidx; i++) {
	(instrs[capturearea[i].iidx].doit) (capturearea[i].w,
					    capturearea[i].x,
					    capturearea[i].y,
					    capturearea[i].z);
    }
    capturemode = WRITEMODE;
    *capturepatch2 |= (ic_ptr - capturepatch2);

}

#ifdef notdef
/* FIXME? */
/*
 * ic_replacebranch is called after code terminating a clause has
 * been emitted.  ptr is a pointer to the beginning of this code.  If
 * the target of the branch instruction just before capturepatch2 is ptr,
 * then the branch instruction is replaced by the code between ptr and ic_ptr.
 *
 * If the branches are not presently short ones or if adding the code will
 * make them longer, the replacement is not done.
 */

void
ic_replacebranch(ptr)
    Code *ptr;
{
    register long replen;
    register Code *p, *q;

    if (!capturepatch2)
	return;

    if (ptr - capturepatch2 > 62)
	return;

    if (((*(capturepatch2 - 1) & 0xff) >> 1) + capturepatch2 != ptr)
	return;

    replen = ic_ptr - ptr - 1;

    if (capturepatch2 - capturepatch1 + replen > 62)
	return;

    for (p = ic_ptr; p >= capturepatch2; p--)
	*(p + replen) = *p;

    ic_ptr += replen;
    if (capturepatch2 <= firstargptr)
	firstargptr += replen;

    for (p = capturepatch2 - 1, q = ic_ptr - replen - 1; q < ic_ptr;)
	*p++ = *q++;

    *(capturepatch1 - 1) += (replen * 2);

}
#endif

static void
ic_1stargalloc()
{
    long   oldmode = capturemode;
    long   i;

    capturemode = READMODE;

    firstargptr = ic_ptr;
    for (i = 0; i < allocidx; i++) {
	(instrs[allocarea[i].iidx].doit) (allocarea[i].w,
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



void
icode(iidx, w, x, y, z)
    int iidx;
    long w, x, y, z;
{
    static long proc_id, proc_arity;
    static long firstargkey;

    if (makeobp)
	f_icode(iidx, w, x, y, z);

    if (iidx < 0) {
	switch (iidx) {
	    case IC_INIT:
		ic_ptr = icode_buf;
		capturemode = WRITEMODE;
		firstargkey = MTP_UNBOUND;
		dstart = icode_buf;
		firstargptr = icode_buf;
		firstargprocessed = 0;
		capturepatch1 = (Code *) 0;
		capturepatch2 = (Code *) 0;
		ic_failaddr = (Code *) 0;
		envsavemask = 0;
		RELOC_IC_INIT
		    break;
	    case IC_ENDCLAUSE:
		proc_id = w;
		proc_arity = x;
		if (firstargptr < dstart)
		    firstargptr = dstart;
		break;
	    case IC_ASSERTA:
		w_asserta(proc_id, proc_arity, icode_buf, ic_ptr - icode_buf,
		   firstargkey, firstargptr - icode_buf, dstart - icode_buf,
			  envsavemask);
		break;
	    case IC_ASSERTZ:
		w_assertz(proc_id, proc_arity, icode_buf, ic_ptr - icode_buf,
		   firstargkey, firstargptr - icode_buf, dstart - icode_buf,
			  envsavemask);
		break;
	    case IC_ADDCLAUSE:
		w_addclause(proc_id, proc_arity, *top_clausegroup,
			    icode_buf, ic_ptr - icode_buf,
		   firstargkey, firstargptr - icode_buf, dstart - icode_buf,
			    envsavemask);
		break;
	    case IC_EXECQUERY:
		w_execquery(icode_buf, ic_ptr - icode_buf);
		break;
	    case IC_EXECCOMMAND:
		w_execcommand(icode_buf, ic_ptr - icode_buf);
		break;
	    case IC_ADDUSE:
		mod_adduse(cur_mod, w);
		break;
	    case IC_ENDMODULE:
		end_mod();
		break;
	    case IC_NEWMODULE:
		new_mod(w);	/* w is token id of new module */
		break;
	    case IC_EXPORTPRED:
		export_pred(cur_mod, w, x);
		break;
	    case IC_1STARG:
		switch (w) {
		    case TP_VO:
			firstargkey = MTP_UNBOUND;
			break;
		    case TP_LIST:
			firstargkey = MTP_LIST;
			break;
		    case TP_INT:
			firstargkey = MMK_INT(z);
			break;
		    case TP_SYM:
			firstargkey = MMK_FUNCTOR(x, y);
			break;
		}
		firstargprocessed = 1;
		break;
	    case IC_BEGINMACRO:
		ic_begin_macro();
		break;
	    case IC_ENDMACRO:
		ic_end_macro(w);
		break;
	    case IC_PUTMACRO:
		ic_put_macro(w);
		break;
	    case IC_CREMODCLOSURE:
		createModuleClosureProcedure(w, x, y);
		break;
	    case IC_ADDTO_AUTOUSE:
		add_default_use(w);
		break;
	    case IC_BEGINALLOC:
		allocidx = 0;
		capturemode = ALLOCMODE;
		break;
	    case IC_ENDALLOC:
		capturemode = WRITEMODE;
		break;
	    case IC_ICRESET:
		break;
	    default:
		fprintf(stderr, "Warning: unrecognized icode command (%d).\n", iidx);
		break;
	}
    }
    else if (iidx == I_END_CAPTURE)
	ic_end_capture(w, x, y, z);
    else if (capturemode == CAPTUREMODE) {
	capturearea[captureidx].iidx = iidx;
	capturearea[captureidx].w = w;
	capturearea[captureidx].x = x;
	capturearea[captureidx].y = y;
	capturearea[captureidx].z = z;
	captureidx++;
    }
    else if (capturemode == MACROMODE) {
	macroarea[macroidx].iidx = iidx;
	macroarea[macroidx].w = w;
	macroarea[macroidx].x = x;
	macroarea[macroidx].y = y;
	macroarea[macroidx].z = z;
	macroidx++;
    }
    else if (capturemode == ALLOCMODE) {
	allocarea[allocidx].iidx = iidx;
	allocarea[allocidx].w = w;
	allocarea[allocidx].x = x;
	allocarea[allocidx].y = y;
	allocarea[allocidx].z = z;
	allocidx++;
	(instrs[iidx].doit) (w, x, y, z);
    }
    else {
	(instrs[iidx].doit) (w, x, y, z);
    }
}
