/*===================================================================*
 |		icode1.c
 |	Copyright (c) 1990-95 Applied Logic Systems, Inc.
 |
 |		-- clause code generator for SPARC
 |
 | Author: Kevin A. Buettner
 | Creation: 8/29/90
 *===================================================================*/

#include <stdio.h>

#include "defs.h"
#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "varproc.h"
#include "module.h"
#include "machinst.h"
#include "codegen.h"
#include "rinfo.h"

#define ICODE(macro,str,addr,obp) void addr PARAMS(( long, long, long, long ));
#include "icodedef.h"
#undef ICODE

/*
 * READMODE, WRITEMODE, CAPTUREMODE, MACROMODE, and ALLOCMODE are the
 * potential values which the variable capturemode may take on.
 */

#define READMODE 0		/* in read mode			*/
#define WRITEMODE 1		/* in write mode		*/
#define CAPTUREMODE 2		/* capture unify instrs		*/
#define MACROMODE 3		/* loading a macro expansion	*/
#define ALLOCMODE 4		/* emitting allocation code	*/

/*
 * CAPTURESIZE is the size of the capture area.  The capture area is where we
 * put the icode instructions in a unify sequence in order to split the
 * instructions into read/write mode later on.
 *
 * There are other capture areas.  One is the macro area -- the area in which
 * different kinds of expansions are temporarily put (math code goes in this
 * area).  The other is the allocation area.  This area is where certain
 * allocation instructions are diverted for possible future duplication if
 * the clause requires it.
 */

#define CAPTURESIZE 500


/*
 * Capture (read/write mode) area
 */

static struct capturestruct {
		int iidx;
		int w,x,y,z;
	} capturearea[CAPTURESIZE];

static int captureidx;
static Code * capturepatch1;	/* patch indices for read/write mode */
static Code * capturepatch2;


/*
 * Macro area
 */

static struct capturestruct macroarea[CAPTURESIZE];
static int macroidx;
Code *ic_macropatch1;
Code *ic_macropatch2;


/*
 * Allocation area
 */

static struct capturestruct allocarea[CAPTURESIZE];
static int allocidx;

/*
 * Other state variables
 */

static int firstargprocessed;	/* 1 if the first arg has been processed,
				 * 0 otherwise.
				 */
static Code *dstart;		/* Pointer to the determinate case
				 * when we don't know anything about
				 * the first argument.
				 */
static Code *firstargptr;	/* Pointer to the determinate code
				 * for the first argument in which
				 * the dereference loop is skipped
				 * because the first argument has
				 * already been dereferenced.
				 */
static long envsavemask;	/* mask corresponding to the top of 
				 * stack of the locations to save
				 * when creating an environment.
				 *
				 * This value is also used to determine
				 * whether a clause requires the
				 * unit clause variations of the
				 * retry and trust code.
				 */

static int backupok;		/*
				 * This is a flag which indicates whether
				 * or not it is permissible to back up to
				 * the previous instruction to get a
				 * delay instruction.
				 *
				 * 0 indicates that it is not permissible.
				 * 1 indicates that it is.
				 */

static int capturemode = WRITEMODE;


/*
 * makeobp is a flag which indicates whether or not we are writing the icode
 * calls out to an obp file or not.
 */

int makeobp;

/*
 * Icode Buffer:
 *	The icode buffer is a large block of memory allocated when the
 *	system starts up to hold clauses that the code generator is
 *	producing prior to putting them in the code area.  If it
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
/* Now declared in icodegen.h; 
Code *ic_ptr;
*/
ic_uptr_type ic_uptr;

#define ICBUFSAFETY	0x100	/* 256 code words */

static Code *icode_buf_end;

static	Code *	ic_deref	PARAMS(( long ));
static	Code	get_delay_instr	PARAMS(( void ));
static	void	ic_uiastr	PARAMS(( char * ));
static	void	ic_begin_macro	PARAMS(( void ));
static	void	ic_end_macro	PARAMS(( int ));
static	void	ic_put_macro	PARAMS(( int ));
static	void	ic_replacebranch PARAMS(( Code * ));
static	void	ic_1stargalloc	PARAMS(( void ));

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


#define ICBUF_OVERFLOW_CHECK						\
	if (ic_ptr > icode_buf_end)					\
		fatal_error(FE_ICODEBUFOVER,0);


/*
 * ic_get_operand is given the base and displacement.  It will emit a load
 * instruction if necessary and return the register number the operand is
 * expected to be in.
 */

int 
ic_get_operand(base, disp, target)
    long base, disp, target;
{
    if (base == 0)
	return disp;		/* disp is the register number */
    else {
	LD(base,imm(disp*4),target)
	return target;
    }
}


/*
 * ic_put_operand is given a base, displacement, and source register.  It will
 * emit a store instruction if necessary.  If a register to register operand
 * is being performed, it will emit a move.
 */

void
ic_put_operand(base, disp, source)
    long base, disp, source;
{
    if (base == 0) {
	if (disp != source) {
	    MOVE(source, disp)
	}
    }
    else {
	ST(source, base, imm(disp*4))
    }
}

/*
 * ic_deref is used to generate a dereference loop.  The readmode patch
 *	address is returned as the value of this function.
 */

static Code *
ic_deref(reg)
    long reg;
{
    LABEL deref, ground;		/* our labels */

	ANDcc(reg,imm(0x03),tmp1)
    BLABDCL(deref)
	BNE(FLAB(ground))
	ANDN(reg,imm(0x03),S)
	LD(S,ZERO,reg)
	CMP(S,reg)
	BNE(BLAB(deref))
	ANDcc(reg,imm(0x03),tmp1)
    
    return ground;			/* return address to patch later */
}


void
move_const(c,dst)
    long c;
    long dst;
{
    if (immLower <= c && c <= immUpper) {
	ADD(ZERO,imm(c),dst)
    }
    else {
	SETHI(hi22(c),dst)
	ADD(dst,imm(lo10(c)),dst)
    }
}


static Code
get_delay_instr()
{
    if (backupok)
	return(*--ic_ptr);
    else
	return(iSETHI(0,ZERO));	/* nop */
}


/*
 * Instruction:	ic_addtosp
 * Function:	adds a number to the stack pointer
 * Parameters:	num	-- number to add
 */

void
ic_addtosp(num,x,y,z)
    long num;
    long x,y,z;				/* dummy parameters */
{
    if (num != 0) {
	num <<= 2;			/* multiply by four */
	if (immLower <= num && num <= immUpper) {
	    ADD(SP,imm(num),SP)
	}
	else {
	    move_const(num,tmp1);
	    ADD(SP,tmp1,SP)
	}
	backupok = 1;
    }
}


/*
 * Instruction:	ic_call
 * Function:	implements the procedure call
 * Parameters:	p	-- token index of procedure to call
 *		a	-- arity of the procedure
 */

void
ic_call(p,a,y,z)
    long p;
    long a;
    long y, z;				/* dummy parameters */
{
    ntbl_entry *ent;

    ent  = w_nameentry(cur_mod,p,a);
    CALL_RELOC(BLAB(ent->call_entry),RELOC_PROC_CALL_WDISP30,0);
    MOVE(E,OldE)			/* move E to OldE */

    backupok = 0;
}


/*
 * Instruction:	ic_execute
 * Function:	implements the execute instruction (call of last goal)
 * Parameters:	p	-- token index of procedure to execute
 *		a	-- arity of procedure
 */

void
ic_execute(p,a,x,y)
    long p;
    long a;
    long x,y;				/* dummy parameters */
{
    Code delay_instr;
    Code *whereto;
    ntbl_entry *ent;

    delay_instr = get_delay_instr();
    ent  = w_nameentry(cur_mod,p,a);
    whereto = ent->exec_entry;
#ifndef PACKAGE
    ic_replacebranch(whereto);
#else  	/* PACKAGE */
    /****** I didn't like this *******
    if (backupok == 0 && capturepatch2 && 
	*capturepatch2 == iBA(ic_ptr-capturepatch2)) {
	*capturepatch2 = iBA(whereto-capturepatch2);
    	RELOC_INFO_INSERT(RELOC_PROC_EXEC_WDISP22,capturepatch2,(long)ent)
    }
    *********/
#endif 	/* PACKAGE */
    CALL_RELOC(BLAB(whereto), RELOC_PROC_EXEC_WDISP30,0);
    ic_put(delay_instr);

    backupok = 0;
}


/*
 * Instruction:	ic_allocate
 * Function:	allocates space for the first call in a multi-goal clause.
 * Parameters:	size	-- combined size of environment and arguments
 *				(in longwords)
 */

void
ic_allocate(size, x,y,z)
    long size;
    long x,y,z;				/* dummy parameters */
{
    ic_addtosp(-size,0,0,0);
    backupok = 1;
}


/*
 * Instruction:	ic_allocate1
 * Function:	allocates space for goal in a clause with one goal
 * Parameters:	size	-- space on stack needed in non-determinate case
 *				(this is usually equal to size of head)
 */

void
ic_allocate1(size, x,y,z)
    long size;
    long x,y,z;				/* dummy parameters */
{
    if (capturemode != READMODE)
	ic_addtosp(-size,0,0,0);
    backupok = 0;
}


/*
 * Instruction:	ic_endallocate1
 * Function:	Records the dstart value
 */

void
ic_endallocate1(x,y,z,w)
    long x,y,z,w;			/* all dummy parameters */
{
    if (capturemode != READMODE) {
	dstart = ic_ptr;
    }
    backupok = 0;
}


/*
 * Instruction:	ic_deallocate
 * Function:	Sets up stack pointer for last call in a multi-goal clause.
 *		Also restores previous environment pointer.
 * Parameters:	size1	-- value in longwords ot subtract from SPB giving
 *			   new stack position when things are non-determinate
 *		size2	-- value in longwords to subtract from E when the
 *			   clause is determinate.
 *		isdeterminateforsure
 *			-- 1 if the compiler knows for certain that the
 *			   clause is determinate at this point; 0 otherwise.
 */

/*
 * Instruction:	ic_deallocate1 through ic_deallocate4
 * Description: These four functions perform the same actions as ic_deallocate;
 *		the only difference is things are allowed to happen between
 *		the various phases for better efficiency.
 */

static LABEL deallocate2patch;

void
ic_deallocate1(w,x,y,z)
    long w,x,y,z;			/* all dummy parameters */
{
    deallocate2patch = (Code *) 0;
    backupok = 0;
}

void
ic_deallocate2(size1,x,y,z)
    long size1;		/* size to use in non-determinate case */
    long x,y,z;
{
    size1 *= -4;		/* adjust for byte offset */

    CMP(E,SPB)
    BCS_a(FLAB(deallocate2patch))
    NOP				/* will fill in later */
    ADD(SPB,imm(size1),SP)
    backupok = 0;
}

void
ic_deallocate3(w,x,y,z)
    long w,x,y,z;			/* all dummy parameters */
{
    /* do nothing */
    backupok = 0;
}

void
ic_deallocate4(size2,x,y,z)
    long size2;
    long x,y,z;				/* dummy paramters */
{
    size2 *= -4;
    if (deallocate2patch) {
	FLABDCL(deallocate2patch)			/* patch BCS_a */
	*(deallocate2patch+1) = iADD(E,imm(size2),SP);	/* overwrite the nop */
	backupok = 0;
    }
    else {
	ADD(E,imm(size2),SP)
	backupok = 1;
    }
}

/*
 * Instruction:	ic_trim
 * Function:	Sets up stack pointer for next call in a multi-goal clause
 *		This code is not used before the first or last goals.  It
 *		also trims some of the environment away when possible.
 * Paramters:	size1	-- value in longwords of environment that must
 *			   persist after the call.
 *		size2	-- value in longwords of size of next goal or
 *			   difference in size of environment previously
 *			   and current size which ever is greater
 *		isdeterminateforsure
 *			-- 1 if the compiler knows for certain that the
 *			   clause is determinate at this point; 0 otherwise
 */

void
ic_trim(size1,size2,isdeterminateforsure, x)
    long size1, size2, isdeterminateforsure;
    long x;				/* dummy parameter */
{
    int totsize;
    size1 *= -4;
    size2 *= -4;
    totsize = size1 + size2;
    
    if (isdeterminateforsure) {

	ADD(E,imm(totsize),SP)
	backupok = 1;
    }
    else {
	LABEL around;

	    CMP(E,SPB)
	    BGU_a(FLAB(around))
	    ADD(SPB,imm(size2),SP)
	    ADD(E,imm(totsize),SP)
	FLABDCL(around)
	backupok = 0;
    }
}


/*
 * Instruction:	ic_proceed
 * Function:	Implements the return from procedure
 * Parameters:	base	-- base from which to get the return address
 *		disp	-- displacement off of base to get return address
 */

void
ic_proceed(base,disp,x,y)
    long base;
    long disp;
    long x,y;				/* dummy parameters */
{
	JMPL(CP,ZERO,ZERO)
	MOVE(OldE,E)
	backupok = 0;
}


/*
 * Instruction:	ic_inline_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *		is usually emitted as a result of a cut as the last goal.
 * Parameters:	none
 */

void
ic_inline_proceed(w,x,y,z)
    long w,x,y,z;			/* all dummy parameters */
{
	LD(E,imm(4),CP)
	JMPL(CP,ZERO,ZERO)
	LD(E,ZERO,E)
	backupok = 0;
}


/*
 * Instruction:	ic_g_uia
 * Function:	Emits code for matching a uia in the head
 * Parameters:	uiastr	-- string corresponding to uia
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */

void
ic_g_uia(uiastr,base,disp, x)
    long uiastr;
    long base, disp;
    long x;				/* dummy parameter */
{
    int reg;
    
    if (UArg1 != (reg = ic_get_operand(base,disp,UArg1))) {
	CALL_RELOC(BLAB(wm_g_uia), RELOC_GVAR_WDISP30,symidx_wm_g_uia);
	MOVE(reg,UArg1)
    }
    else {
	CALL_RELOC(BLAB(wm_g_uia),RELOC_GVAR_WDISP30,symidx_wm_g_uia);
	NOP
    }
    ic_uiastr((char *)uiastr);
    backupok = 0;	/* don't want to use data as an instr */
}

#ifdef PACKAGE

coff_init_g_uia()
{
  INSERT_SYM(symidx_wm_g_uia,		 	"_wm_g_uia");
}

#endif /* PACKAGE */

/*
 * Instruction:	ic_p_uia
 * Function:	Emits code for putting down a uia in the body
 * Parameters:	uiastr		-- string corresponding to the UIA
 *		base		-- index of the base register
 *		disp		-- displacement from the base register
 */

void
ic_p_uia(uiastr,base,disp,x)
    long uiastr;
    long base, disp;
    long x;				/* dummy parameter */
{
    CALL_RELOC(BLAB(wm_p_uia),RELOC_GVAR_WDISP30,symidx_wm_p_uia);
    NOP
    
    ic_uiastr((char *)uiastr);
    ic_put_operand(base,disp,UArg1);
    backupok = 1;
}

#ifdef PACKAGE

coff_init_p_uia()
{
  INSERT_SYM(symidx_wm_p_uia,		 	"_wm_p_uia");
}

#endif /* PACKAGE */

static void
ic_uiastr(s)
    char *s;
{
    register int l;
    l = strlen(s)+1;
    if (l & 3)
	l = (l & ~3) + 4;		/* round up */
    
    l += 4;				/* add one longword for the fence */
    l >>= 2;				/* convert to longwords */
    ic_put(MMK_FENCE(l));
    strcpy((char *) ic_ptr, s);
    ic_ptr += (l-1);

    /*
     * Make sure that there are all nulls following the last null in the last
     * word.
     */
    
    s = (char *) (ic_ptr-1);
    while (*s++);
    *s++ = 0;
    *s++ = 0;
    *s++ = 0;
}


/* 
 * Instruction:	ic_g_sym
 * Function:	Emits code corresponding to the symbol part of Warren's
 *		get_constant instruction
 * Parameters:	tokid	-- token index
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */

void
ic_g_sym(tokid, base, disp, x)
    long tokid, base, disp;
    long x;				/* dummy parameter */
{
    int reg;
    int delay_instr;

    if (UArg1 != (reg = ic_get_operand(base,disp,UArg1))) {
	MOVE(reg, UArg1)
    }

    move_const(MMK_SYM(tokid), UArg2);

    delay_instr = *--ic_ptr;

    CALL_RELOC(BLAB(wm_g_sym), RELOC_GVAR_WDISP30,symidx_wm_g_sym);

    ic_put(delay_instr);
    backupok = 0;
}

#ifdef PACKAGE

coff_init_g_sym()
{
  INSERT_SYM(symidx_wm_g_sym,		 	"_wm_g_sym");
}

#endif /* PACKAGE */

/*
 * Instruction:	ic_g_int
 * Function:	Emits code corresponding to the integer part of Warren's
 *		get_constant instruction
 * Parameters:	i	-- integer to get
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */

void
ic_g_int(i,base,disp, x)
    long i, base, disp;
    long x;				/* dummy parameter */
{
    int reg, delay_instr;

    if (UArg1 != (reg = ic_get_operand(base,disp,UArg1))) {
	MOVE(reg, UArg1)
    }

    move_const(MMK_INT(i), UArg2);
    delay_instr = *--ic_ptr;
    CALL_RELOC(BLAB(wm_g_int), RELOC_GVAR_WDISP30,symidx_wm_g_int);
    ic_put(delay_instr);
    backupok = 0;
}

#ifdef PACKAGE

coff_init_g_int()
{
  INSERT_SYM(symidx_wm_g_int,		 	"_wm_g_int");
}

#endif /* PACKAGE */

/*
 * Instruction:	ic_move
 * Function:	emits a move instruction
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 *
 * Note:	If either sbase or dbase is equal to REGS, then the
 *		displacement is the register number to use.
 */

void
ic_move(sbase,sdisp, dbase,ddisp)
    long sbase, sdisp, dbase, ddisp;
{
    int sreg;
    if (dbase)
	sreg = ic_get_operand(sbase,sdisp,tmp1);
    else
	sreg = ic_get_operand(sbase,sdisp,ddisp);
    ic_put_operand(dbase,ddisp,sreg);
    backupok = 1;
}


/*
 * Instruction:	ic_g_value
 * Function:	Emits code to unify to operands
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note:	If either sbase or dbase is equal to REGS, then the
 *		displacement is the register number to use.
 */


void
ic_g_value(sbase,sdisp,dbase,ddisp)
    long sbase, sdisp, dbase, ddisp;
{
    int delay_instr;

    ic_move(sbase,sdisp,REGS,UArg1);
    ic_move(dbase,ddisp,REGS,UArg2);
    delay_instr = *--ic_ptr;
    CALL_RELOC(BLAB(wm_unify), RELOC_GVAR_WDISP30,symidx_wm_unify);
    ic_put(delay_instr);
    backupok = 0;
}

#ifdef PACKAGE

coff_init_g_value()
{
  INSERT_SYM(symidx_wm_unify, "_wm_unify");
}

#endif /* PACKAGE */

/*
 * structure_offset is the offset from the beginning of a structure to
 * access the next argument of a structure.  It is initialized by 
 * get_structure, get_list, put_structure, and put_list.  It is advanced
 * by the unify_ instructions.
 */

static structure_offset;


/*
 * Instruction:	ic_g_list
 * Function:	Emits code to unify an argument with a list
 * Parameters:	base	-- base register
 *		disp	-- displacement
 * Note:	If base is equal to REGS then the displacement is the
 *		register number to use.
 */

void
ic_g_list(base,disp, x,y)
    long base, disp;
    long x,y;			/* dummy parameters */
{
    int reg;
    LABEL l1, l2;

    if (capturemode == WRITEMODE) {
	reg = ic_get_operand(base,disp,UArg1);

	capturepatch1 = ic_deref(reg);

	/*
	 * We must bind the variable with a list pointer
	 */
	
	ADD(H,imm(MTP_LIST),UArg1)
	CMP(S,HB)
	BCC(FLAB(l1))		/* Branch on greater or equal, unsigned */
	ST(UArg1,S,imm(0))
	CMP(S,SPB)
	BCS(FLAB(l2))		/* Branch on less than, unsigned */
	NOP
	SUB(TR,imm(4),TR)
	ST(S,TR,imm(0))

	FLABDCL(l1)
	FLABDCL(l2)
    }
    else {
	CMP(tmp1,imm(MTP_LIST))
	BE_a(FLAB(l1))
	NOP
	JMPL(Fail,ZERO,ZERO)
	NOP
	if (!firstargprocessed)
	    ic_1stargalloc();
	FLABDCL(l1)

    }
    structure_offset = 0;
    backupok = 0;		/* have labels at end of sequence */
}


/*
 * Instruction: ic_g_structure
 * Function:	emits code to unify a structure with an argument
 * Parameters:	funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 *
 * Note:	If base is TREG, then the displacement is the register number
 *		to use.
 */

void
ic_g_structure(funcid,arity,base,disp)
    long funcid, arity;
    long base, disp;
{
    LABEL l1, l2;
    int reg;
    int functor = MMK_FUNCTOR(funcid,arity);

    if (capturemode == WRITEMODE) {
	move_const(functor,UArg2);
	reg = ic_get_operand(base,disp,UArg1);
	capturepatch1 = ic_deref(reg);
	ADD(H,imm(MTP_STRUCT),UArg1)
	CMP(S,HB)
	BCC(FLAB(l1))		/* Branch on greater or equal, unsigned */
	ST(UArg1,S,imm(0))
	CMP(S,SPB)
	BCS(FLAB(l2))		/* Branch on less than, unsigned */
	NOP
	SUB(TR,imm(4),TR)
	ST(S,TR,imm(0))

	FLABDCL(l1)
	FLABDCL(l2)
	ST(UArg2,H,imm(0))
	backupok = 1;
    }
    else {
	CMP(tmp1,imm(MTP_STRUCT))
	BE_a(FLAB(l2))
	LD(S,imm(0),UArg1)
	BLABDCL(l1)
	JMPL(Fail,ZERO,ZERO)
	NOP
	if (!firstargprocessed) {
	    ic_1stargalloc();
	    LD(S,imm(0),UArg1)
	    move_const(functor,UArg2);
	}
	FLABDCL(l2)
	CMP(UArg1, UArg2)
	BNE_a(BLAB(l1))
	NOP
	backupok = 0;		/* can't backup over a delay instr */
    }
    structure_offset = 4;
}


/*
 * Instruction:	ic_u_sym
 * Function:	emits code for unify_symbol
 * Parameters:	sym	-- symbol to unify element of structure with
 */

void
ic_u_sym(sym,x,y,z)
    long sym;
    long x,y,z;		/* dummy parameters */
{
    Code delay_instr;
    int c = MMK_SYM(sym);

    if (capturemode == WRITEMODE) {
	move_const(c,UArg1);
	ST(UArg1,H,imm(structure_offset))
	backupok = 1;
    }
    else {
	LD(S,imm(structure_offset),UArg1)
	move_const(c,UArg2);
	delay_instr = *--ic_ptr;
	CALL_RELOC(BLAB(wm_u_sym), RELOC_GVAR_WDISP30,symidx_wm_u_sym);
	ic_put(delay_instr);
	backupok = 0;		/* can't backup over delay instr */
    }
    structure_offset += 4;
}

#ifdef PACKAGE

coff_init_u_sym()
{
  INSERT_SYM(symidx_wm_u_sym,		 	"_wm_u_sym");
}

#endif /* PACKAGE */

/*
 * Instruction:	ic_u_int
 * Function:	emits code for unify_integer
 * Parameters:	i	-- integer to unify element of structure with
 */

void
ic_u_int(i, x,y,z)
    long i;
    long x, y, z;
{
    Code delay_instr;
    int c = MMK_INT(i);

    if (capturemode == WRITEMODE) {
	move_const(c,UArg1);
	ST(UArg1,H,imm(structure_offset))
	backupok = 1;
    }
    else {
	LD(S,imm(structure_offset),UArg1)
	move_const(c,UArg2);
	delay_instr = *--ic_ptr;
	CALL_RELOC(BLAB(wm_u_int), RELOC_GVAR_WDISP30,symidx_wm_u_int);
	ic_put(delay_instr);
	backupok = 0;
    }
    structure_offset += 4;
}

#ifdef PACKAGE

coff_init_u_int()
{
  INSERT_SYM(symidx_wm_u_int,		 	"_wm_u_int");
}

#endif /* PACKAGE */

/*
 * Instruction:	ic_u_var
 * Function:	Implements the unify_variable instruction (for head matching)
 * Parameters:	base		-- base register
 *		disp		-- displacement
 */

void
ic_u_var(base, disp, argn, inbody)
    long base, disp, argn, inbody;
{
    int reg;
    LABEL l1;


    if (base)
	reg = tmp1;
    else
	reg = disp;

    if (capturemode == WRITEMODE) {
	/*
	 * The following code generates trailing code if the destination is an
	 * environment cell.  This is necessary for GC to work properly.
	 */
	
	if (base == EREG && inbody) {
		ADD(E,imm(disp*4),tmp2)
		CMP(tmp2,SPB)
		BCS(FLAB(l1))
		ADD(H,imm(structure_offset),reg)
		SUB(TR,imm(4),TR)
		ST(tmp2,TR,imm(0))
	    FLABDCL(l1)
	}
	else {
		/* The following instruction is done in the delay slot
		 * of the conditional branch (above).  It sets up reg
		 * with the heap location to make a variable.
		 */
		ADD(H,imm(structure_offset),reg)
	}
	ic_put_operand(base,disp,reg);
	ST(reg,reg,imm(0))
    }
    else {
	/* Read Mode */
	LD(S,imm(structure_offset),reg)
	ic_put_operand(base,disp,reg);
    }

    structure_offset += 4;
    backupok = 1;
}


/*
 * Instruction:	ic_u_val
 * Function:	Implements the unify_value instruction (for head matching)
 * Parameters:	base	-- base register
 *		disp	-- displacement
 */

void
ic_u_val(base,disp, y,z)
    long base, disp;
    long y,z;			/* dummy parameters */
{
    Code delay_instr;
    int reg;

    if (capturemode == READMODE) {
	LD(S,imm(structure_offset),UArg1)
	reg = ic_get_operand(base,disp,UArg2);
	if (reg != UArg2) {
	    MOVE(reg,UArg2)
	}
	delay_instr = *--ic_ptr;
	CALL_RELOC(BLAB(wm_unify), RELOC_GVAR_WDISP30,symidx_wm_unify);
	ic_put(delay_instr);
	backupok = 0;
    }
    else {
	reg = ic_get_operand(base,disp,UArg1);
	ST(reg,H,imm(structure_offset))
	backupok = 1;
    }

    structure_offset += 4;
}


/*
 * Instruction:	ic_u_lval
 * Function:	Implements the unify_local_value instruction
 * Parameters:	base	-- base register
 *		disp	-- displacement
 */


void
ic_u_lval(base,disp,y,z)
    long base, disp;
    long y,z;			/* dummy paramters */
{
    int reg;
    Code delay_instr;

    if (capturemode == READMODE) {
	/* Same as ic_u_val */
	LD(S,imm(structure_offset),UArg1)
	reg = ic_get_operand(base,disp,UArg2);
	if (reg != UArg2) {
	    MOVE(reg,UArg2)
	}
	delay_instr = *--ic_ptr;
	CALL_RELOC(BLAB(wm_unify), RELOC_GVAR_WDISP30,symidx_wm_unify);
	ic_put(delay_instr);
    }
    else {
	if (UArg1 != (reg = ic_get_operand(base,disp,UArg1))) {
	    MOVE(reg, UArg1)
	}
	CALL_RELOC(BLAB(wm_u_lval), RELOC_GVAR_WDISP30,symidx_wm_u_lval);
	ADD(H,imm(structure_offset),UArg2)
    }

    structure_offset += 4;
    backupok = 0;		/* have delay instrs at end of sequences */
}

#ifdef PACKAGE

coff_init_u_lval()
{
  INSERT_SYM(symidx_wm_u_lval, 		"_wm_u_lval");
}

#endif /*PACKAGE */


/*
 * Instruction:	ic_u_void
 * Function:	Emits instructions for handling variables in structure that
 *		occur only once in a clause.
 * Parameters:	None
 */

void
ic_u_void(x,y,z,w)
    long x,y,z,w;		/* all dummy paramters */
{
    if (capturemode == READMODE) {
	/* do nothing */
	/* backupok will stay the same */
    }
    else {
	ADD(H,imm(structure_offset),tmp1)
	ST(tmp1,tmp1,imm(0))
	backupok = 1;
    }

    structure_offset += 4;
}


/*
 * Instruction:	ic_p_unsafe
 * Function:	Emits a put_unsafe_value instruction
 * Parameters:	sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 */

void
ic_p_unsafe(sbase,sdisp,dbase,ddisp)
    long sbase,sdisp,dbase,ddisp;
{
    Code delay_instr;
    int reg;

    if (UArg1 != (reg = ic_get_operand(sbase,sdisp,UArg1))) {
	MOVE(reg,UArg1)
    }

    delay_instr = *--ic_ptr;
    CALL_RELOC(BLAB(wm_p_unsafe), RELOC_GVAR_WDISP30,symidx_wm_p_unsafe);
    ic_put(delay_instr);
    ic_put_operand(dbase,ddisp,UArg1);
    backupok = 1;
}

#ifdef PACKAGE

coff_init_p_unsafe()
{
  INSERT_SYM(symidx_wm_p_unsafe,	 	"_wm_p_unsafe");
}
  
#endif /* PACKAGE */

/*
 * Instruction:	ic_p_int
 * Function:	Moves the prolog representation of an integer to the 
 *		destination.
 * Parameters:	i	-- integer
 *		base	-- destination base register
 *		disp	-- destination displacement
 */

void
ic_p_int(i,base,disp,x)
    long i, base, disp;
    long x;			/* dummy parameter */
{
    int reg;

    if (base)
	reg = UArg1;
    else
	reg = disp;

    move_const(MMK_INT(i),reg);

    ic_put_operand(base,disp,reg);
    backupok = 1;
}


/*
 * Instruction:	ic_p_sym
 * Function:	Moves the prolog representation of a symbol to the 
 *		destination.
 * Parameters:	sym	-- symbol
 *		base	-- destination base register
 *		disp	-- destination displacement
 */

void
ic_p_sym(sym,base,disp,x)
    long sym, base, disp;
    long x;
{
    int reg;

    if (base)
	reg = UArg1;
    else
	reg = disp;
    
    move_const(MMK_SYM(sym),reg);

    ic_put_operand(base,disp,reg);
    backupok = 1;
}


/*
 * Instruction: ic_p_yvar
 * Function:	Installs an unbound variable at the given environment position
 *		and puts a reference to this variable in the destination.
 * Parameters:	ebase	-- base register needed to access environment
 *		edisp	-- displacement to access variable
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note:	If ebase or dbase is REGS, the the displacement is the number
 *		of the temporary to use.
 */

void
ic_p_yvar(ebase,edisp,dbase,ddisp)
    long ebase,edisp,dbase,ddisp;
{
    int reg;

    if (dbase)
	reg = UArg1;
    else
	reg = ddisp;
    
    ADD(ebase,imm(edisp*4),reg)
    ST(reg,reg,imm(0))

    ic_put_operand(dbase,ddisp,reg);
    backupok = 1;
}


/*
 * Instruction:	ic_init_yvar1
 * Function:	Initializes an environment variable.  All environment variables
 *		must be initialized prior to the first call for garbage
 *		collection purposes.
 * Parameters:	ebase	-- base register needed to access environment
 *		edisp	-- displacement for accessing the variable
 */

void
ic_init_yvar1(ebase,edisp,x,y)
    long ebase, edisp;
    long x,y;			/* dummy parameters */
{
    ADD(ebase,imm(edisp*4),UArg1)
    ST(UArg1,UArg1,imm(0))
    backupok = 1;
}


/*
 * Instruction:	ic_init_yvar2
 * Function:	Called after ic_init_yvar1 to efficiently initialize other
 *		environment variables.
 * Parameters:	incr	-- adjustment needed.  Due to the way the code 
 *			   generator is written on machines with a post-
 *			   increment addressing mode, incr is one less than
 *			   it needs to be
 */

void
ic_init_yvar2(incr,x,y,z)
    long incr;
    long x,y,z;			/* dummy parameters */
{
    ADD(UArg1,imm(4*(incr+1)),UArg1)
    ST(UArg1,UArg1,imm(0))
    backupok = 1;
}


/*
 * Instruction:	ic_p_xvar
 * Function:	Installs an unbound variable on the top of the heap and
 *		put a reference to this variable in the destination.  The
 *		top of the heap is move up by one location.
 * Parameters:	base	-- base register for desination
 *		disp	-- displacement to get the destinationi
 * Note:	If base is REGS, then the displacement is the number of
 *		the temporary to use.
 */

void
ic_p_xvar(base,disp,x,y)
    long base,disp;
    long x,y;			/* dummy parameters */
{
    ST(H,H,imm(0))
    ic_put_operand(base,disp,H);
    ADD(H,imm(4),H)
    backupok = 1;
}


/*
 * Instruction:	ic_p_list
 * Function:	Emits code to put a pointer to the list structure in an
 *		argument
 * Parameters:	base	-- base register
 *		disp	-- displacement
 * Note:	If base is REGS, then the displacement is the number of the
 *		temporary to use.
 */

void
ic_p_list(base,disp,x,y)
    long base;
    long disp;
    long x, y;
{
    int reg;

    if (base)
	reg = UArg1;
    else
	reg = disp;
    
    structure_offset = 0;
    ADD(H,imm(MTP_LIST),reg)

    ic_put_operand(base,disp,reg);
    backupok = 1;
}


/*
 * Instruction:	ic_p_structure
 * Function:	Emits code to put pointer to a structure in an argument
 * Parameters:	funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacment
 * Note:	If base is REGS, then the displacment is the number of the
 *		temporary to use.
 */

void
ic_p_structure(funcid,arity,base,disp)
    long funcid, arity;
    long base, disp;
{
    int reg;

    if (base)
	reg = UArg1;
    else
	reg = disp;
    
    move_const(MMK_FUNCTOR(funcid,arity),UArg1);
    ST(UArg1,H,imm(0))
    ADD(H,imm(MTP_STRUCT),reg)
    ic_put_operand(base,disp,reg);

    structure_offset = 4;
    backupok = 1;
}


/* 
 * Instruction:	ic_endstruct
 * Function:	Emits code to update the heap pointer after a sequence of
 *		unify instructions for put_structure or put_list.
 * Parameters:	None.
 */

void
ic_endstruct(x,y,z,w)
    long w,x,y,z;		/* dummy parameters */
{
    ADD(H,imm(structure_offset),H)
    backupok = 1;
}


/*
 * Instruction:	ic_do_cut
 * Function:	Emits code for performing the cut operation
 * Parameters:	base	-- base register from which the environment is accessed
 *		disp	-- number of longwords to add to base to get to the
 *			   environment
 */


void
ic_docut(base,disp,z,w)
    long base, disp;
    long z,w;			/* dummy parameters */
{
    CALL_RELOC(BLAB(wm_docut), RELOC_GVAR_WDISP30,symidx_wm_docut);
    ADD(base,imm(4*disp),UArg1)
    backupok = 0;
}

#ifdef PACKAGE

coff_init_docut()
{
  INSERT_SYM(symidx_wm_docut, "_wm_docut");
}

#endif /* PACKAGE */

/*
 * Instruction:	ic_cut_proceed
 * Function:	Implements cut as the first, and only goal
 * Parameters:	base	-- base from which to get the return address
 *		disp	-- displacement of return address off of base
 */

void
ic_cut_proceed(base,disp,y,z)
    long base, disp;
    long y, z;
{
    ADD(base,imm(4*disp),UArg1)
    MOVE(OldE,E)
    BA_RELOC(BLAB(wm_docut), RELOC_GVAR_WDISP22,symidx_wm_docut);
    SUB(CP,imm(8),RET)
    backupok = 0;
}

/*
 * Instruction:	ic_deallocate_cut_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *		is usually emitted as a result of a cut as the last goal.
 * Parameters:	None.
 */

void
ic_deallocate_cut_proceed(x,y,z,w)
    long x, y, z, w;
{
    LD(E,imm(4),RET)
    MOVE(E,UArg1)
    LD(E,imm(0),E)
    BA_RELOC(BLAB(wm_docut), RELOC_GVAR_WDISP22,symidx_wm_docut);
    SUB(RET,imm(8),RET)
    backupok = 0;

}


/*
 * Instruction:	ic_cutmacro
 * Function:	Emits code needed for performing cuts within a cut macro
 * Parameters:	ebase	-- environment base register
 *		edisp	-- displacement to get to end of arguments
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 */

void
ic_cutmacro(ebase,edisp,dbase,ddisp)
    long ebase, edisp, dbase, ddisp;
{
    int reg;

    if (dbase)
	reg = UArg1;
    else
	reg = ddisp;
    
    ADD(ebase,imm(edisp*4),tmp1)
#ifdef notdef
#ifndef PACKAGE
    move_const(wm_heapbase,reg);
#else 	/* PACKAGE */
    SETHI_RELOC(hi22(&wm_heapbase),reg,RELOC_GVAR_HI22,symidx_wm_heapbase);
    LD_RELOC(reg,imm(lo10(&wm_heapbase)),reg,RELOC_GVAR_LO10,
		     symidx_wm_heapbase);
#endif	/* PACKAGE */
#endif
    SUB(HeapBase,tmp1,reg)
    SLL(reg,imm(4),reg)
    ADD(reg,imm(MTP_INT),reg)
    ic_put_operand(dbase,ddisp,reg);
    backupok = 1;
}


/*
 * Instruction:	ic_callinfo
 * Function:	Installs the garbage collect call information.  This call
 *		information consists of executable instructions from which
 *		the number of arguments to the procedure and the argument
 *		mask may be extracted.  The instructions are not meant
 *		to be executed, but if they are, nothing untoward will
 *		happen as they are nops.
 *
 *		The SPARC implementation uses to "sethi" instructions with
 *		a destination of r0 to encode this information.  The sethi
 *		instruction with destination zero is a nop, but 22 bits of
 *		the opcode are usable for encoding whatever we wish.
 *
 *		The low sixteen bits of the first opcode is used to encode
 *		the usage mask.  The low sixteen bits of second sethi
 *		instruction encodes the number of words back to the start
 *		of the clause.  Bits sixteen through twenty (five bits)
 *		in the first and second word encode the number of arguments
 *		with the first word containing the low order bits and the
 *		second the high order.
 *
 *		Notice that one bit in each word is wasted.  This is so
 *		that the format will correspond exactly to the representation
 *		used for the 88k implementation.  The instruction used on
 *		the 88k is an addu and the bit pattern which encodes the
 *		addu is different, but the representation of the data is
 *		the same.  If there is a reason for it, we can in the future
 *		change the implementation so that we use these extra bits.
 *
 * Parameters:	msk	-- arguments usage mask
 *		nargs	-- number of arguments
 */

void
ic_callinfo(msk,nargs,envsize,w)
    long msk;
    long nargs;
    long envsize;
    long w;			/* dummy argument */
{
    if (!envsavemask) {
	envsavemask = ((msk<<2) | 0x3) & 0x1f;
    }

    ic_put(-BLAB(icode_buf));	/* this will look like an unimp instruction */
    ic_put((envsize <<16) | nargs);
    ic_put(msk);
    backupok = 0;		/* this has to stay put */
}


/*
 * Instruction:	ic_start_capture
 * Function:	Indicates the start of a sequence in the head for matching
 *		list or structure in which two separate patches of code
 *		(read mode and write mode) need to be emitted
 *
 * Parameters:	None.
 */

void
ic_start_capture(x,y,z,w)
    long x, y, z, w;
{
    captureidx = 0;
    capturemode = CAPTUREMODE;
}


/*
 * Instruction:	ic_begin_macro
 * Function:	Begins the diversion of the instructions to another area
 *		for later installation in the implementation of a macro
 *		(such as inline arithmetic).
 * Parameters:	None.
 * Note:	This is actually an icode command.
 */

static void
ic_begin_macro()
{
    macroidx = 0;
    capturemode = MACROMODE;
}



/*
 * Instruction:	ic_end_macro
 * Function:	Indicates the end of a macro sequence
 * Parameters:	keepit	-- indicates if the macro will actually be kept
 */

static void
ic_end_macro(keepit)
    int keepit;
{
    capturemode = WRITEMODE;
    if (keepit) {
	Code delay_instr = get_delay_instr();
	CMP(Safety,ZERO)
	BGU(FLAB(ic_macropatch1))
	ic_put(delay_instr);
	backupok = 0;
    }
}


#define ICODE(macro,str,addr,obp) {addr},

static struct {
    void  (*doit) PARAMS(( long, long, long, long ));
} instrs[] = {
#include "icodedef.h"
};

static void
ic_put_macro(islast)
    int islast;
{
    int i;

    if (!islast) {
	BA_a(FLAB(ic_macropatch2))
    }

    FLABDCL(ic_macropatch1)

    backupok = 0;
    for (i=0; i<macroidx; i++) {
	(instrs[macroarea[i].iidx].doit)(macroarea[i].w,
					 macroarea[i].x,
					 macroarea[i].y,
					 macroarea[i].z);
	ICBUF_OVERFLOW_CHECK
    }

    if (!islast) {
	FLABDCL(ic_macropatch2)
    }
    backupok = 0;
}


void
ic_end_capture(x,y,z,w)
    long x,y,z,w;			/* dummy parameters */
{
    int i;

    capturemode = WRITEMODE;

    for (i=0; i<captureidx; i++) {
	(instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					  capturearea[i].x,
					  capturearea[i].y,
					  capturearea[i].z);
	ICBUF_OVERFLOW_CHECK
    }

    BA(FLAB(capturepatch2))
    ADD(H,imm(structure_offset),H)
    FLABDCL(capturepatch1)

    capturemode = READMODE;

    backupok = 0;
    for (i=0; i<captureidx; i++) {
	(instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					  capturearea[i].x,
					  capturearea[i].y,
					  capturearea[i].z);
	ICBUF_OVERFLOW_CHECK
    }

    capturemode = WRITEMODE;
    FLABDCL(capturepatch2)
    backupok = 0;
}

/*
 * ic_replacebranch is called by the code implementing the execute instruction.
 * It is designed to replace the branch emitted by ic_end_capture when this
 * branch branches to the execute instruction.
 */

static void
ic_replacebranch(whereto)
    Code *whereto;
{
    if (backupok == 0 && capturepatch2 && 
	*capturepatch2 == iBA(ic_ptr-capturepatch2))
	*capturepatch2 = iBA(whereto-capturepatch2);
}


static void
ic_1stargalloc()
{
    int oldmode = capturemode;
    int i;
    capturemode = READMODE;

    firstargptr = ic_ptr;
    backupok = 0;
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
    backupok = 0;
}


void
icode(iidx, w, x, y, z)
    int iidx; 
    long w, x, y, z;
{
    static int proc_id, proc_arity;
    static int firstargkey;


    if (makeobp)
	f_icode(iidx,w,x,y,z);
    
    if (iidx < 0) {
	switch (iidx) {
	    case IC_INIT :
		ic_ptr = icode_buf;
		capturemode = WRITEMODE;
		firstargkey = MTP_UNBOUND;
		dstart = icode_buf;
		firstargptr = icode_buf;
		firstargprocessed = 0;
		capturepatch1 = (Code *) 0;
		capturepatch2 = (Code *) 0;
		envsavemask = 0;
		backupok = 0;	/* shouldn't backup past start of buffer */
		RELOC_IC_INIT
		break;
	    case IC_ENDCLAUSE :
		proc_id = w;
		proc_arity = x;
		if (firstargptr < dstart)
		    firstargptr = dstart;
		break;
	    case IC_ASSERTA :
		w_asserta(proc_id,proc_arity,icode_buf,ic_ptr-icode_buf,
			  firstargkey,firstargptr-icode_buf,dstart-icode_buf,
			  envsavemask);
		break;
	    case IC_ASSERTZ :
		w_assertz(proc_id,proc_arity,icode_buf,ic_ptr-icode_buf,
			  firstargkey,firstargptr-icode_buf,dstart-icode_buf,
			  envsavemask);
		break;
	    case IC_ADDCLAUSE :
		w_addclause(proc_id,proc_arity,*top_clausegroup,
			    icode_buf,ic_ptr-icode_buf,
			    firstargkey,firstargptr-icode_buf,dstart-icode_buf,
			    envsavemask);
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
		fprintf(stderr,"Warning: unrecognized icode command (%d).\n",iidx);
		break;
	}
    }
    else if (iidx == I_END_CAPTURE)
	ic_end_capture(w,x,y,z);
    else if (capturemode == CAPTUREMODE) {
	capturearea[captureidx].iidx	= iidx;
	capturearea[captureidx].w	= w;
	capturearea[captureidx].x	= x;
	capturearea[captureidx].y	= y;
	capturearea[captureidx].z	= z;
	captureidx++;
    }
    else if (capturemode == MACROMODE) {
	macroarea[macroidx].iidx	= iidx;
	macroarea[macroidx].w		= w;
	macroarea[macroidx].x		= x;
	macroarea[macroidx].y		= y;
	macroarea[macroidx].z		= z;
	macroidx++;
    }
    else if (capturemode == ALLOCMODE) {
	allocarea[allocidx].iidx= iidx;
	allocarea[allocidx].w		= w;
	allocarea[allocidx].x		= x;
	allocarea[allocidx].y		= y;
	allocarea[allocidx].z		= z;
	allocidx++;
	(instrs[iidx].doit)(w,x,y,z);

	ICBUF_OVERFLOW_CHECK
    }
    else {
	(instrs[iidx].doit)(w,x,y,z);

	ICBUF_OVERFLOW_CHECK
    }

}

#ifdef PACKAGE
coff_init_icode1()
{
    coff_init_g_uia();
    coff_init_p_uia();
    coff_init_g_sym();
    coff_init_g_int();
    coff_init_g_value();
    coff_init_u_sym();
    coff_init_u_int();
    coff_init_u_lval();
    coff_init_p_unsafe();
    coff_init_docut();
}
#endif /* PACKAGE */
