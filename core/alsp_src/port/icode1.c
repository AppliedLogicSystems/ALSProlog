/*===================================================================*
 |		icode1.c
 |	Copyright (c) 1987-95 Applied Logic Systems, Inc.
 |
 |		-- clause code generator for portable BYTE/THREADED
 |
 | Author: Kevin A. Buettner
 | Creation: 2/12/87
 | 03/22/87 K.Buettner -- icode.c split into icode1.c,
 |                        icode2.c, and icode.h
 | 08/18/92 P.Raman    -- related icode files merged into
 |                        icode1.c and icode2.c
 *===================================================================*/

#include "defs.h"
#include "coerce.h"
#include "icom.h"
#include "module.h"
#include "labels.h"
#include "compile.h"
#include "icodegen.h"
#include "icode.h"		/* generated in bld dir */
#include "istr.h"
#include "wintcode.h"
#include "machinst.h"

extern int system_debugging;

#ifdef KERNAL
#define MAXCALLS    80
#define ICBUFSIZE  3024
#else
#define MAXCALLS    512
#define ICBUFSIZE 32768
#endif /* KERNAL */

Code *dstart;	/* Where to go for a determinate start of
				 * clause
				 */

int makeobp;

/*
 * data structure for storing gc call information
 */

static struct {
    Code *patchaddr;
    long  argenvsize;
    long  argmask;
} callinfo[MAXCALLS];

static int callidx;		/* call index                   */

/* 1 if the first argument has been processed.  0 otherwise. */

int   firstargprocessed;

/*
 * Pointer to the determinate code for the first argument in which
 * the dereference code is skipped because A1 has the dereferenced
 * first argument.
 */

Code *firstargptr;

int   capturemode;		/* not used in Port code */

/*
 * The icode buffer and buffer pointer
 */

Code  icode_buf[ICBUFSIZE];
/*  Now declared in icodegen.h; 
  Code *ic_ptr;  */

ic_uptr_type ic_uptr;

/*
 * Previous instruction pointer for doing minimial peephole optimization
 */

Code *ic_pptr;

/*---------------------------------------------------------------------------
 * The "Portable" version has no temporary registers.  Therefore, the compiler
 * will allocate stack when it needs a temporary.  The problem with this is
 * that we are inundated with instructions which decrement SP.  Rather than
 * changing SP at all, we will simply record the offset to add to SP to get
 * to where it really should be and update SP with this value before a call
 * or execute instruction.
 *--------------------------------------------------------------------------*/

static long sp_disp;

#define BLKSIZE(obj) (sizeof(obj)/sizeof(Code))

#define ICPrint(iidx,w,x,y,z) \
  if (system_debugging) \
  printf("%s(%ld,%ld,%ld,%ld)\n", \
		 (iidx >= 0 ? posic[iidx] : negic[-iidx-1]), \
		 w,x,y,z);

#define ICODE(macro,str,addr,obp) void addr ( long, long, long, long );
#include "icodedef.h"

static	void	ic_illegal	( long, long, long, long );
static	void	ic_uiastr	( char * );
static	void	ic_p_const	( long, long, long );
static	void	ic_backpatch	( Code *, int );
static	void	addtosp		( long );

#undef ICODE
#define ICODE(macro,str,addr,obp) {addr},

static struct {
    void  (*doit) ( long, long, long, long );
} instrs[] = {

#include "icodedef.h"
    { ic_illegal }
};


void
ic_illegal(long x, long y, long z, long w)
{
    printf("Illegal icode instruction\n");
}

void
ic_start_capture(long x, long y, long z, long w)
{
}

void
ic_end_capture(long x, long y, long z, long w)
{
}


/* ic_punch is used to put one opcode at a given address */

void
ic_punch(Code *ptr, Code data)
{
    *ptr = abinst(data);
}

/* ic_put_align puts  down an opcode and moves the icode-buffer
 * pointer to following long-word aligned location
 */

void
ic_put_align(Code data)
{
    *ic_ptr++ = abinst(data);
    ic_ptr += (sizeof (PWord) - sizeof (Code)) / sizeof (Code);
}

/*
 * put a register_code, source, destination into icode buffer
 *
 * base = 1, SP, register_code = 0
 * base = 2, E , register_code = 1
 * base = 3, H , register_code = 2
 */

void
ic_put_reg(Code base, long offset)
{
    PWord *longptr;

    if (base == 1 /* SP */)
	offset += sp_disp;
    *ic_ptr++ = base - 1;
    longptr = (PWord *) ic_ptr;
    *longptr++ = offset;
    ic_ptr = (Code *) longptr;
}

/*
 * ic_putl puts a longword data into icode buffer
 */

void
ic_putl(PWord data)
{
    PWord *ptr = (PWord *) ic_ptr;

    *ptr++ = data;
    ic_ptr = (Code *) ptr;
}



/*
 * ic_backpatch is used to fix up the byte displacement of a JMP or Jcc
 *      instruction.  If using a byte displacement can't be used because the
 *      forward reference is too far away, the instructions below the branch
 *      instruction are shifted forward in memory by one word and a word
 *      displacement is installed.  The single parameter where is the index
 *      of the word after the branch instruction.  This is the location to
 *      install the word displacement, if necessary.  Fixing up a byte
 *      displacement involves backing up one word and oring in the displacement.
 *      Note that we assume that the byte displacment starts out as zero which
 *      is necessary for either the short fixup or the word fixup.
 */

static void
ic_backpatch(Code *where, int conditional)
{
    PWord *dst = (PWord *) where;

    if (where)
	*(dst - 1) = (PWord) (ic_ptr - where) + sizeof (PWord) / sizeof (Code);
}


void
ic_callinfo(long mask, long nargs, long envsize, long w)
{
    callinfo[callidx].patchaddr = ic_ptr;
    callinfo[callidx].argenvsize = (envsize << 16) | nargs;
    callinfo[callidx].argmask = mask;

    callidx++;
    if (callidx >= MAXCALLS) {
    	fprintf(stderr, "overflowed call buffer\n");
    	exit(-1);
    }
    ic_puti(W_GCMAGIC);
    ic_putl(0L);
}


/*
 * addtosp is the only call which really calls W_ADDTOSP
 */

static void
addtosp(long num)
{
    num += sp_disp;
    if (num != 0) {
	ic_puti(W_ADDTOSP);
	ic_putl(num);
    }
    sp_disp = 0;
}


/*
 * Instruction: ic_call
 * Function:    implements the procedure call
 * Parameters:  p       -- token index of procedure to call
 *                      a       -- arity of procedure
 */

void
ic_call(long p, long a, long y, long z)
{
    if (sp_disp) {
	ic_puti(W_ADDTOSP_CALL);
	ic_putl(sp_disp);
	sp_disp = 0;
    }
    else
	ic_puti(W_CALL);
    ic_putl((PWord)w_nameentry((PWord) cur_mod, p, (int) a)->exec_entry);
}


/*
 * Instruction: ic_execute
 * Function:    implements the execute instruction (call of last goal)
 * Parameters:  p       -- token index of procedure to execute
 *                              a       -- arity of procedure
 */

void
ic_execute(long p, long a, long x, long y)
{
    if (sp_disp) {
	ic_puti(W_ADDTOSP_EXECUTE);
	ic_putl(sp_disp);
	sp_disp = 0;
    }
    else
	ic_puti(W_EXECUTE);
    ic_putl((PWord)w_nameentry((PWord) cur_mod, p, (int) a)->exec_entry);
}


/*
 * Instruction: ic_allocate
 * Function:    allocates and space for first call in a
 *                              multi-goal clause.
 * Parameters:  size    -- size of arguments (in longwords)
 */

void
ic_allocate(long size, long x, long y, long z)
{
    ic_addtosp(-size, x, y, z);
}


/*
 * Instruction: ic_allocate1
 * Function:    allocates space for goal in a clause with one goal
 * Parameters:  size    -- space on stack needed in non-determinate case
 *                         (this is usually equal to the size of the
 *                          head)
 */

void
ic_allocate1(long size, long x, long y, long z)
{
    addtosp(-size);
}

/*
 * Instruction: ic_endallocate1
 * Function:    If needed, we set down where to jump into the
 *                              clause in the case of a determinate entry.
 * Parameters:  none
 */

void
ic_endallocate1(long x, long y, long z, long w)
{
    LABEL(dstart)
}


/*
 * Instruction: ic_deallocate1 through ic_deallocate4
 * Description: These four functions perform the same actions
 *                              as ic_deallocate.
 */

static Code *deallocate2patch;
static Code *deallocate3patch;
static PWord *size2patch;
static int my_isdetflag;

void
ic_deallocate1(long w, long x, long y, long z)
{
    deallocate2patch = deallocate3patch = (Code *) 0;
    my_isdetflag = 1;
}

void
ic_deallocate2(long size1, long x, long y, long z)
{
    my_isdetflag = 0;
    ic_puti(W_DEALLOCATE2);
    ic_putl(size1);
    ic_putl(0L);
    size2patch = (PWord *) ic_ptr;;
    ic_putl(0L);
    LABEL(deallocate2patch);
    sp_disp = 0;
}


void
ic_deallocate3(long w, long x, long y, long z)
{
}

void
ic_deallocate4(long size2, long x, long y, long z)
{
    if (my_isdetflag) {
	ic_puti(W_DEALLOCATE4);
	ic_putl(size2);
	sp_disp = 0;
    }
    else {
	*(size2patch - 1) = size2;	/* backpatch size2 */
	ic_backpatch(deallocate2patch, 0);
    }
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
 */


void
ic_trim(long size1, long size2, long isdeterminateforsure, long x)
{
    if (isdeterminateforsure) {
	ic_puti(W_TRIM1);
	ic_putl(size1 + size2);
    }
    else {
	ic_puti(W_TRIM2);
	ic_putl(size1);
	ic_putl(size2);
    }
    sp_disp = 0;
}

/*
 * Instruction: ic_proceed
 * Function:    Implements the return from procedure
 * Parameters:  base    -- base from which to get the return address
 *              disp    -- displacement off of base of return address
 *                         (in longwords)
 */

void
ic_proceed(long base, long disp, long y, long z)
{
    ic_puti(W_PROCEED);
}


/*
 * Instruction: ic_inline_proceed
 * Function:    Deallocates an environment and then returns.  This instruction
 *              usually is emitted as a result of a macro as the last goal.
 *
 * Parameters:  none
 */

void
ic_inline_proceed(long w, long x, long y, long z)
{
    ic_puti(W_PROCEED);
}

/*
 * Instruction: ic_init_yvar1
 * Function:    Initializes an environment variable.  All environment variables
 *              must be initialized prior to the first call for garbage
 *              collection purposes.
 * Parameters:  ebase   -- base register needed to access environment
 *              edisp   -- displacement for accessing the variable
 */

void
ic_init_yvar1(long ebase, long edisp, long x, long y)
{
    ic_puti(W_INIT_YVAR1);
    ic_putl(edisp);
}

/*
 * Instruction: ic_init_yvar2
 * Function:    Called after ic_init_yvar1 to efficiently initialize other
 *              environment variables.
 * Parameters:  incr    -- number when multiplied by 4 needed to add to A0
 *              for proper address.  Note that when the environment variables
 *              go sequentially, incr will be zero due to the use of the
 *              post increment on the move.
 */

void
ic_init_yvar2(long incr, long x, long y, long z)
{
    ic_puti(W_INIT_YVAR2);
    ic_putl(incr);
}


/*
 * Instruction: ic_g_uia
 * Function:    Emits code for matching a uia in the head
 * Parameters:  uiastr          -- string corresponding to uia
 *              base            -- index of base register
 *              disp            -- displacement from base register
 */

void
ic_g_uia(long uiastr, long base, long disp, long x)
{
    ic_puti(W_G_UIA);
    ic_put_reg(base, disp);

    /* Lay down UIA after instruction */
    ic_uiastr((char *) uiastr);
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
ic_g_sym(long tokid, long base, long disp, long x)
{
    ic_puti(W_G_SYM);
    ic_put_reg(base, disp);
    ic_putl(MMK_SYM(tokid));
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
ic_g_int(long i, long base, long disp, long x)
{
    ic_puti(W_G_INT);
    ic_put_reg(base, disp);
    ic_putl(MMK_INT(i));
}

/*
 * Instruction: ic_g_value
 * Function:    emits code to unify the two operands.
 * Parameters:  sbase   -- source base register
 *              sdisp   -- source displacement
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 */

void
ic_g_value(long sbase, long sdisp, long dbase, long ddisp)
{
    ic_puti(W_G_VALUE);
    ic_put_reg(sbase, sdisp);
    ic_put_reg(dbase, ddisp);
}

/*
 * Instruction: ic_g_list
 * Function:    emits code to unify an argument with a list
 * Parameters:  base    -- base register
 *              disp    -- displacement
 */

void
ic_g_list(long base, long disp, long x, long y)
{
    if (base == 1) {
	disp += sp_disp;
	ic_puti(W_G_LIST_SP);
	ic_putl(disp);
    }
    else {
	if (2 <= disp && disp <= 5) {
	    ic_puti(W_G_LIST_E_p2 + disp - 2);
	}
	else {
	    ic_puti(W_G_LIST_E);
	    ic_putl(disp);
	}
    }
}

/*
 * Instruction: ic_g_structure
 * Function:    emits code to unify a structure with an argumetn
 * Parameters:  funcid  -- token index of functor
 *              arity   -- arity of structure
 *              base    -- base register
 *              disp    -- displacement
 */

void
ic_g_structure(long funcid, long arity, long base, long disp)
{
    if (base == 1) {
	disp += sp_disp;
	ic_puti(W_G_STRUCT_SP);
    }
    else
	ic_puti(W_G_STRUCT_E);
    ic_putl(disp);
    ic_putl(MMK_FUNCTOR(funcid, arity));
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
ic_p_uia(long uiastr,long  base, long disp, long x)
{
    ic_puti(W_P_UIA);
    ic_put_reg(base, disp);
    ic_uiastr((char *) uiastr);
}

/*
 * Instruction: ic_p_unsafe
 * Function:    emits a put_unsafe_value instruction
 * Parameters:  sbase   -- source base register
 *              sdisp   -- source displacement
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 */

void
ic_p_unsafe(long sbase, long sdisp, long dbase, long ddisp)
{
    ic_puti(W_P_UNSAFE);
    ic_put_reg(sbase, sdisp);
    ic_put_reg(dbase, ddisp);
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
ic_p_int(long i, long base, long disp, long x)
{
    ic_puti(W_P_SYM);
    ic_p_const(MMK_INT(i), base, disp);
}

/*
 * Instruction ic_p_sym
 * Function:    Moves the prolog representation of a symbol to the
 *              destination.
 * Parameters:  sym     -- symbol
 *              base    -- destination base register
 *              disp    -- destination displacement
 */

void
ic_p_sym(long sym, long base, long disp, long x)
{
    ic_puti(W_P_SYM);
    ic_p_const(MMK_SYM(sym), base, disp);
}


static void
ic_p_const(long con, long base, long disp)
{
    ic_put_reg(base, disp);
    ic_putl(con);
}

/*
 * Instruction: ic_p_yvar
 * Function:    Installs an unbound variable at the given environment position
 *              and puts a reference to this variable in the destination.
 * Parameters:  ebase   -- base register needed to access environment
 *              edisp   -- displacement to access variable
 *              dbase   -- destination base register
 *              ddisp   -- destination displacement
 */

void
ic_p_yvar(long ebase, long edisp, long dbase, long ddisp)
{
    if (ebase == 1)
	edisp += sp_disp;
    if (dbase == 1)
	ddisp += sp_disp;

    if (ebase == 2 && dbase == 1) {
	ic_puti(W_P_YVAR_ES);
	ic_putl(edisp);
	ic_putl(ddisp);
    }
    else if (ebase == 1 && dbase == 1) {
	ic_puti(W_P_YVAR_SS);
	ic_putl(edisp);
	ic_putl(ddisp);
    }
    else if (ebase == 2 && dbase == 2) {
	ic_puti(W_P_YVAR_EE);
	ic_putl(edisp);
	ic_putl(ddisp);
    }
    else if (ebase == 1 && dbase == 2) {
	ic_puti(W_P_YVAR_SE);
	ic_putl(edisp);
	ic_putl(ddisp);
    }
}

/*
 * Instruction: ic_p_xvar
 * Function:    Installs an unbound variable on the top of the heap and
 *              puts a reference to this variable in the destination.
 *              The top of the heap is moved up by one location.
 * Parameters:  base    -- base register for destination
 *              disp    -- displacement to get to destination
 */

void
ic_p_xvar(long base, long disp, long x, long y)
{
    ic_puti(W_P_XVAR);
    ic_put_reg(base, disp);
}

/*
 * Instruction: ic_p_list
 * Function:    emits code to put a pointer to a list structure in an argument
 * Parameters:  base    -- base register
 *              disp    -- displacement
 */

void
ic_p_list(long base, long disp, long x, long y)
{
    ic_puti(W_P_LIST);
    ic_put_reg(base, disp);
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
ic_p_structure(long funcid, long arity, long base, long disp)
{
    ic_puti(W_P_STRUCTURE);
    ic_put_reg(base, disp);
    ic_putl(MMK_FUNCTOR(funcid, arity));
}

/*
 * Instruction: ic_endstruct
 * Function:    emits code to update the heap pointer after a sequence of
 *              unify instructions for put_structure or put_list.
 * Parameters:  None.
 */

void
ic_endstruct(long x, long y, long z, long w)
{
}


/*
 * Instruction: ic_addtosp
 * Function:    adds a number to the stack pointer
 * Parameters:  num     -- number to add
 * Code summary:
 *      lea     sp,[sp+size]
 */

void
ic_addtosp(long num, long x, long y, long z)
{
    sp_disp += num;
}

/*
 * ic_uiastr(s)
 *
 * Lay down what is needed to create the UIA represented by the string s.
 *
 */

static void
ic_uiastr(char *s)
{
    register long l, i;
    register char *cptr;

    /* Calculate length in PWords (including null byte) and round up */
    i = strlen(s) + 1;
    l = i / sizeof (PWord);

    /* If remainder is non-zero, got extra word going on */
    if ((i %= sizeof (PWord))) {
	i = sizeof (PWord) - i;
	l++;
    }

    l++;			/* Include a fence */

    ic_putl(l + 1);		/* number of PWords to follow */

    ic_putl(MMK_FENCE(l));

    /* put down the string. Had to be done this way so that OBP files worked
     * correctly on the 386i
     */

    cptr = (char *) ic_ptr;
    while ( (*cptr++ = *s++) ) ;
    while (i--)
	*cptr++ = 0;
    ic_ptr = (Code *) cptr;

    ic_putl(MMK_FENCE(l));
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
 */

void
ic_move(long sbase, long sdisp, long dbase, long ddisp)
{
    if (sbase == 1)
	sdisp += sp_disp;
    if (dbase == 1)
	ddisp += sp_disp;

    if (sbase == 2 && dbase == 1) {
	if (0 <= sdisp && sdisp <= 6 && -4 <= ddisp && ddisp <= 5)
	    ic_puti(W_MOVE_ES_m2_m4 + 10*sdisp + ddisp + 24);
	else {
	    ic_puti(W_MOVE_ES);
	    ic_putl(sdisp);
	    ic_putl(ddisp);
	}
    }
    else if (sbase == 1 && dbase == 1) {
	ic_puti(W_MOVE_SS);
	ic_putl(sdisp);
	ic_putl(ddisp);
    }
    else if (sbase == 2 && dbase == 2) {
	ic_puti(W_MOVE_EE);
	ic_putl(sdisp);
	ic_putl(ddisp);
    }
    else if (sbase == 1 && dbase == 2) {
	ic_puti(W_MOVE_SE);
	ic_putl(sdisp);
	ic_putl(ddisp);
    }
}


/*
 * Instruction: ic_u_sym
 * Function:    emits code for unify_symbol
 * Parameters:  sym     -- symbol to unify element of structure with
 *
 */

void
ic_u_sym(long sym, long n, long y, long z)
{
    ic_puti(W_U_SYM);
    ic_putl(MMK_SYM(sym));
}

/*
 * Instruction: ic_u_int
 * Function:    emits code for unify_integer
 * Parameters:  i       -- integer to unify element of structure with
 *
 */

void
ic_u_int(long i, long n, long y, long z)
{
    ic_puti(W_U_INT);
    ic_putl(MMK_INT(i));
}

/*
 * Instruction: ic_u_var
 * Function:    Implements the unify_variable instruction
 *                              (for head matching)
 * Parameters:  base
 *                              disp
 *                              n       -- The position in the struct of this object
 */

void
ic_u_var(long base, long disp, long n, long body)
{
    if (base == 1) {
	disp += sp_disp;
	if (disp == 2 && ic_pptr && *ic_pptr == abinst(W_U_VAR_SP_m1))
	    ic_punch(ic_pptr,W_U_VAR_SP_m1_p2);
	else if (-3 <= disp && disp <= 4)
	    ic_puti(W_U_VAR_SP_m4+disp+4);
	else {
	    ic_puti(W_U_VAR_SP);
	    ic_putl(disp);
	}
    }
    else if (base == 2) {
	ic_puti(W_U_VAR_E);
	ic_putl(disp);
    }
}

/*
 * Instruction: ic_u_val
 * Function:    Implements the unify_value instruction
 *                              for head matching)
 * Parameters:  base
 *              disp
 *              n
 */

void
ic_u_val(long base, long disp, long n, long z)
{
    if (base == 1) {
	disp += sp_disp;
	if (-4 <= disp && disp <= 4)
	    ic_puti(W_U_VAL_SP_m4+disp+4);
	else {
	    ic_puti(W_U_VAL_SP);
	    ic_putl(disp);
	}
    }
    else if (base == 2) {
	ic_puti(W_U_VAL_E);
	ic_putl(disp);
    }
}

/*
 * Instruction: ic_u_lval
 * Function:    Implements the unify_local_value instruction
 * Parameters:  base
 *              disp
 */

void
ic_u_lval(long base, long disp, long n, long z)
{
    ic_puti(W_U_LVAL);
    ic_put_reg(base, disp);
}

/*
 * Instruction: ic_u_void
 * Function:    emits instructions for handling variables in structure
 *              that occur only once in a clause
 * Parameters:  none
 */

void
ic_u_void(long x, long y, long z, long w)
{
    ic_puti(W_U_VOID);
}


/*
 * Instruction: ic_docut
 * Function:    emits code for performing the cut operation
 * Parameters:  base    -- base register from which the environment is accessed
 *              disp    -- number of longwords to add to base to get to the
 *                         environment
 */

void
ic_docut(long base, long disp, long z, long w)
{
    addtosp(0);
    ic_puti(W_DOCUT);
    ic_callinfo(0L, 0L, 0L, 0L);
}

/*
 * Instruction: ic_cut_proceed
 * Function:    Implements cut as the first, and only goal.
 * Parameters:
 *              base    -- base from which to get the return address
 *              disp    -- displacement off of SP of return address
 *                         (in longwords)
 */

void
ic_cut_proceed(long base, long disp, long y, long z)
{
    addtosp(0);
    ic_puti(W_CUT_PROCEED);
}


/*
 * Instruction: ic_deallocate_cut_proceed
 * Function:    Deallocates an environment and then returns.  This instruction
 *              usually is emitted as a result of a cut as the last goal.
 * Parameters:  none
 */

void
ic_deallocate_cut_proceed(long w, long x, long y, long z)
{
    addtosp(0);
    ic_puti(W_CUT_PROCEED);	/* same as deallocate_cut_proceed */
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
ic_cutmacro(long ebase, long edisp, long dbase, long ddisp)
{
    ic_puti(W_CUTMACRO);
    ic_put_reg(ebase, edisp);
    ic_put_reg(dbase, ddisp);
}


void
icode(int iidx, long w, long x, long y, long z)
{
    static PWord proc_id;
    static int proc_arity;
    static PWord firstargkey;
    register int i;

#ifdef OBP
    if (makeobp)
	f_icode(iidx, w, x, y, z);
#endif

    if (iidx < 0) {
	ICPrint(iidx, w, x, y, z);

	switch (iidx) {
	    case IC_INIT:
		ic_ptr = icode_buf;
		
		memset(icode_buf, -1, sizeof(Code)*ICBUFSIZE);

		callidx = 0;

		firstargkey = MTP_UNBOUND;
		firstargprocessed = 0;

		/* Initialize a couple of labels to the start of the clause.
		 * These may be moved by other code, but maybe not.
		 */

		LABEL(firstargptr);
		LABEL(dstart);

		sp_disp = 0;
		ic_pptr = (Code *) 0;	/* no previous pointer */

		break;

	    case IC_ENDCLAUSE:
		proc_id = w;
		proc_arity = x;

		if (firstargptr < dstart)
		    firstargptr = dstart;

		ic_putl(-1);
		
		for (i = 0; i < callidx; i++) {
		    *((BigOffset *) (callinfo[i].patchaddr + 1)) =
			ic_ptr - callinfo[i].patchaddr;

		    ic_putl(callinfo[i].argenvsize);
		    ic_putl(callinfo[i].argmask);
		}

		/* -1 not allowed as a valid callinfo */
		ic_putl(-1);

		/* Put down distance back to the beginning */
		ic_putl(ic_ptr - icode_buf);

		break;

	    case IC_ASSERTA:
		w_asserta(proc_id, proc_arity,
			  icode_buf, (int) (ic_ptr - icode_buf),
			  firstargkey, (int) (firstargptr - icode_buf),
			  (int) (dstart - icode_buf), (long) 0);
		break;

	    case IC_ASSERTZ:
		w_assertz(proc_id, proc_arity,
			  icode_buf, (int) (ic_ptr - icode_buf),
			  firstargkey, (int) (firstargptr - icode_buf),
			  (int) (dstart - icode_buf), (long) 0);
		break;

	    case IC_ADDCLAUSE:
		w_addclause(proc_id, proc_arity, *top_clausegroup,
			    icode_buf, (int) (ic_ptr - icode_buf),
			    firstargkey, (int) (firstargptr - icode_buf),
			    (int) (dstart - icode_buf), (long) 0);
		break;

	    case IC_EXECQUERY:
		w_execquery(icode_buf, ic_ptr - icode_buf);
		break;

	    case IC_EXECCOMMAND:
		w_execcommand(icode_buf, ic_ptr - icode_buf);
		break;

	    case IC_ADDUSE:
		mod_adduse(cur_mod, (int) w);
		break;

	    case IC_ENDMODULE:
		end_mod();
		break;

	    case IC_NEWMODULE:
		new_mod(w);	/* w is token id of new module */
		break;

	    case IC_EXPORTPRED:
		export_pred((PWord) cur_mod, w, (int) x);
		break;

	    case IC_1STARG:
		switch ((int) w) {
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
		break;

	    case IC_ENDMACRO:
		break;

	    case IC_PUTMACRO:
		break;

	    case IC_CREMODCLOSURE:
		createModuleClosureProcedure(w, (int) x, y);
		break;

	    case IC_ADDTO_AUTOUSE:
		add_default_use((int) w);
		break;

	    case IC_ADDTO_AUTONAME:
		add_default_proc(w, (int) x);
		break;

	    case IC_BEGINALLOC:
		break;

	    case IC_ENDALLOC:
		break;

	    case IC_ICRESET:
		break;

	    default:
		fprintf(stderr, "Warning: unrecognized icode command (%d).\n", iidx);
		break;
	}
    }
    else if (iidx == I_END_CAPTURE) {
	ICPrint(I_END_CAPTURE, w, x, y, z);
    }
    else {
	Code *tp = ic_ptr;
	ICPrint(iidx, w, x, y, z);

	(*instrs[iidx].doit) (w, x, y, z);
	if (tp != ic_ptr)
	    ic_pptr = tp;
#ifdef KERNAL
	if (ic_ptr > icode_buf+ICBUFSIZE) {
	  printf("%p %p\n", ic_ptr, icode_buf+ICBUFSIZE);
	  fprintf(stderr, "overflowed ic buffer\n");
	  /*exit(-1);*/
	}
#endif /* KERNAL */
    }
}
