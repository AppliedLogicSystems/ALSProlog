/*===================================================================*
 |		icode2.c
 |	Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |		-- more stuff to emit instructions
 |
 | Author:	Kevin A. Buettner
 | Creation:	9/12/90--
 | Revsion History:
 *===================================================================*/

#include "defs.h"
#include "compile.h"
#include "icode.h"
#include "wintcode.h"
#include "module.h"
#include "codegen.h"
#include "machinst.h"
#include "rinfo.h"
#include "pckgcoff.h"

#define CLEAR(ptr,off,mask) *(long *)((char *)(ptr)+(off)) &= (mask)

static	void	ic_install_emask	PARAMS(( int ));
static	Code	get_target_instruction	PARAMS(( Code ** ));

extern	void	wm_overflow0	PARAMS(( void ));
extern	void	wm_overflow1	PARAMS(( void ));
extern	void	wm_overflow2	PARAMS(( void ));
extern	void	wm_overflow3	PARAMS(( void ));

static Code * ovtab[] = {
    (Code *) wm_overflow0,
    (Code *) wm_overflow1,
    (Code *) wm_overflow2,
    (Code *) wm_overflow3
};

/*
 * ic_install_overflow_call is used to initialize the overflow field in a
 * procedure table entry (ntbl_entry).  It also fills in the call_entry
 * field
 *
 *	This portion of the name entry is assumed to have the following
 *	structure:
 *
 *	call	overflow
 *	add	RET,	(disp to exec entry),	RET
 *
 * It is assumed that the call entry immediately follows this entry and
 * so the following instruction is also stored:
 *
 *	add	RET, (gc info size), CP
 *
 * Note:
 *	This code differs from the 88k implementation in that the instruction
 *	to set E from SP is done in the execute entry.  On the 88k, the delay
 *	slot in the execute code has this instruction.  On the SPARC
 *	implementation, the delay slot contains something else.
 */

void
ic_install_overflow_call(n)
    ntbl_entry *n;
{
    int disp;

    Code *oldptr = ic_ptr;
    ic_ptr = n->overflow;

    disp = BLAB(n->exec_entry)<<2;
    CALL(BLAB(ovtab[(n->nargs > NAREGS) ? NAREGS : n->nargs]))
    ADD(RET,imm(disp),RET)

    /*
     * The overflow entry is now filled in.  The ic_ptr ought to be pointing
     * at the call entry.  Fill it in.
     *
     * The first thing that we do will be to set CP.  RET (r15) is set by
     * the call instruction (which brings us here) to the address of the
     * call instruction.  There is an instruction in the calls delay slot
     * and there are three 32-bit words which form the garbage collection
     * information.  So we must advance RET a total of 20 bytes to get CP
     * set to the next real instruction to execute upon return.
     *
     */
    ADD(RET,imm(20),CP)
    
    ic_ptr = oldptr;
}

#ifdef PACKAGE

static long ovtab_symidx[] = {
        symidx_wm_overflow0,
        symidx_wm_overflow1,
        symidx_wm_overflow2,
        symidx_wm_overflow3
};


void
coff_init_overflow()
{
  INSERT_SYM(symidx_wm_overflow0, 	"_wm_overflow0");
  INSERT_SYM(symidx_wm_overflow1, 	"_wm_overflow1");
  INSERT_SYM(symidx_wm_overflow2, 	"_wm_overflow2");
  INSERT_SYM(symidx_wm_overflow3, 	"_wm_overflow3");
}

int
coff_pckg_overflow(ent)
     ntbl_entry *ent;
{
  long ov_symidx;
  unsigned long *cp;

  ov_symidx = ovtab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs];
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(ov_symidx),P_R_WDISP30,0);

  cp = (unsigned long *)(ent->overflow);
  COFF_LONG_RAWDATA(*cp & ~MASK30);
  COFF_LONG_RAWDATA(*(cp+1));

  return(1);
}

#endif  

/*
 * ic_install_call_entry is given a procedure entry point and does nothing
 * with it since ic_install_overflow_call does it already.
 */

void
ic_install_call_entry(n)
    ntbl_entry *n;
{
    /* do nothing */
}

#ifdef PACKAGE

int
coff_pckg_call_entry(ent)
	 ntbl_entry *ent;
{
  COFF_RAWDATA(ent->call_entry,CALLENTRYSIZE_BYTES);
  return(1);
}

#endif


/*
 * ic_install_normal_exec_entry is used to install the overflow checking
 * code in the execute entry slot for a procedure entry.
 */

void
ic_install_normal_exec_entry(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->exec_entry;

    SUB(TR,H,tmp1)
    CMP(tmp1,Safety)
    BCS(BLAB(n->overflow))
    MOVE(SP,E)

    ic_ptr = oldptr;
}

#ifdef PACKAGE

int
coff_pckg_exec_entry(ent)
	 ntbl_entry *ent;
{

  COFF_RAWDATA(ent->exec_entry,EXECENTRYSIZE_BYTES);

  return(1);
}

#endif
	
/*
 * ic_install_spy is passed a procedure entry and a subroutine branch is 
 * installed to the spy checking code at the execute entry point.
 *
 * The call to dbg_spycheck on this implementation replaces the stack overflow
 * checking code.  This check must be done in dbg_spycheck
 * 
 */

extern	void	dbg_spycheck	PARAMS(( void ));

void
ic_install_spy(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->exec_entry;

    CALL(BLAB(dbg_spycheck))
    NOP
    BCS(BLAB(n->overflow))
    MOVE(SP,E)
    
    ic_ptr = oldptr;
}


/*
 * ic_install_libbreak is passed a procedure entry and an interrupt number.  
 * The wm_interrupt_caught variable is set to the interrupt number and
 * a branch back to the overflow code is installed in the code area.
 *
 * It is then up to the interrupt handling code to determine what to do with
 * the interrupt.  The original purpose of this code is to handle the library
 * loading code.  It is parameterized, however, for other applications.
 */

void
ic_install_libbreak(n,i)
    ntbl_entry *n;
    int i;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

#ifndef PACKAGE
    move_const(i,UArg1);
#else
	/* we use this alternate form of move_const
	 * for packaging so that we have a fixed
	 * offset to addresses that have to be relocated
	 * in subsequent instructions.
	 */
    SETHI(hi22((long)i),UArg1)
    ADD(UArg1,imm(lo10((long)i)),UArg1)
#endif 	/* PACKAGE */
    SETHI(hi22((long)&wm_interrupt_caught),tmp1)
    ST(UArg1,tmp1,imm(lo10((long)&wm_interrupt_caught)))
    BA(BLAB(n->overflow))
    SUB(ZERO,imm(1),Safety)

    ic_ptr = oldptr;
}

#ifdef PACKAGE

#define wm_interrupt_caught_hi22_OFFSET  8
#define wm_interrupt_caught_lo10_OFFSET  12

void
coff_init_libbreak()
{
  INSERT_SYM(symidx_wm_interrupt_caught, "_wm_interrupt_caught");
  
}

int
coff_pckg_libbreak(ent)
	 ntbl_entry *ent;
{
  long *cp;

  cp = (long *)(ent->code);
  COFF_RAWDATA(cp,wm_interrupt_caught_hi22_OFFSET);
  (char *)cp += wm_interrupt_caught_hi22_OFFSET;
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_interrupt_caught),
						P_R_HI22,0);
  COFF_LONG_RAWDATA(*cp++ & ~MASK22 );

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_interrupt_caught),
						P_R_LO10,0);
  COFF_LONG_RAWDATA(*cp++ & ~MASK10 );

  COFF_RAWDATA(cp,CODESIZE_BYTES-((char *)cp-(char *)(ent->code)));

  return(1);
}

#endif

/*
 * ic_install_decr_icount is passed a procedure entry and a subroutine
 * call to dbg_decr_icount is installed.  This sequence is will take up
 * exactly the same amount of space as the normal execute entry
 */

extern	void	dbg_decr_icount	PARAMS(( void ));

void
ic_install_decr_icount(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->exec_entry;

    CALL(BLAB(dbg_decr_icount))
    NOP					
    BCS(BLAB(n->overflow))
    MOVE(SP,E)

    ic_ptr = oldptr;

}


/*
 * ic_install_resolve_ref is given a procedure entry point and fills in the
 * code region with instructions necessary to resolve the (undefined)
 * reference.
 */

extern	void	wm_resolve_ref	PARAMS(( void ));

void
ic_install_resolve_ref(n)
    ntbl_entry *n;
{
    int disp;

    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    /*
     * Put down the subroutine branch to the appropriate version of
     * wm_resolve_ref.
     */
    
    CALL(BLAB(wm_resolve_ref))


    /*
     * We want to set UArg1 to point to the procedure entry.  The following
     * code which goes into the delay slot will accomplish this.
     */
    
    disp = (int) (char *) (((ntbl_entry *) 0)->code);
    SUB(RET,imm(disp),UArg1)

    ic_ptr = oldptr;
}

#ifdef PACKAGE

static ntbl_entry resolve_ref_template;

int
coff_init_resolve_ref()
{
  long *cp;

  INSERT_SYM(symidx_wm_resolve_ref,	"_wm_resolve_ref");

  ic_install_resolve_ref(&resolve_ref_template);
  CLEAR(resolve_ref_template.code, 0, ~MASK30);
}

int
coff_pckg_resolve_ref(ent)
	 ntbl_entry *ent;
{
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_resolve_ref),P_R_WDISP30,0);
  COFF_RAWDATA(resolve_ref_template.code, CODESIZE_BYTES);

  return(1);
}

#endif
  
/*
 * ic_install_emask is given an emask and lays down code (to the current
 * ic_ptr positions) for the mask.
 */

static void
ic_install_emask(emask)
    int emask;
{
    if (emask & EMSK_OLDE) {
	ST(OldE,E,imm(0))
    }

    if (emask & EMSK_CP) {
	ST(CP,E,imm(4))
    }

    if (emask & EMSK_A1) {
	ST(A1,E,imm(8))
    }

    if (emask & EMSK_A2) {
	ST(A2,E,imm(12))
    }

    if (emask & EMSK_A3) {
	ST(A3,E,imm(16))
    }
}


/*
 * ic_install_jmp is given a procedure entry and puts instructions in the code
 * field to jump to the start of a clause.  The exact code laid down will
 * depend on the value of emask.
 */

void
ic_install_jmp(n,clausestart,emask)
    ntbl_entry *n;
    Code *clausestart;
    int emask;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    ic_install_emask(emask);

    if (ic_ptr == n->code) {
	CALL(BLAB(clausestart))
	NOP
    }
    else {
	Code delay_instr = *--ic_ptr;
	CALL(BLAB(clausestart))
	ic_put(delay_instr);
    }

    ic_ptr = oldptr;
}

#ifdef PACKAGE

int
coff_pckg_code_jmp(ent)
	 ntbl_entry *ent;
{
  Code *paddr;
  long offs, *cp;
  
  /*
   * this procedure is called for packaging only if
   * the code to be packaged was generated by above
   * procedure with "emask" as 0 and the "if" test
   * in the above procedure succeeded. "coff_pckg_code_single"
   * is used to package the code generated by "else" part.
   * 
   */
  cp = (long *)(ent->code);
  offs = *cp & MASK30;
  if (offs & 0x20000000)
	offs |= ~MASK30;
  paddr = (Code *) (cp + offs);
  COFF_RELOC_SYMBOL(builtin_name(paddr),P_R_WDISP30,0);
  COFF_LONG_RAWDATA(*cp++ & ~MASK30);
  COFF_RAWDATA(cp,CODESIZE_BYTES-4);

  return(1);
}

int
coff_pckg_code_single(ent,procname)
	 ntbl_entry *ent;
	 char *procname;
{
  int  emask;
  int  emaskcodesize;
  unsigned long *p;
  char buf[64];

  p = (unsigned long *) (ent->code);
  emask = emaskCode(ent->first_clause);
  emaskcodesize = 0;
  if (emask & EMSK_OLDE)
	emaskcodesize++;
  if (emask & EMSK_CP)
	emaskcodesize++;
  if (emask & EMSK_A1)
	emaskcodesize++;
  if (emask & EMSK_A2)
	emaskcodesize++; 
  if (emask & EMSK_A3)
	emaskcodesize++;
  if (emaskcodesize > 1) {
	/* we subtract 1 because an instruction is sucked into delay slot */
	COFF_RAWDATA((char *)p,(emaskcodesize-1)*sizeof(long));
	p += (emaskcodesize-1);
  }
  sprintf(buf,"%s_%x_%s",
		  procname,clauseId(ent->first_clause),"dstart");
  COFF_RELOC_SYMBOL(buf,P_R_WDISP30,0);
  COFF_LONG_RAWDATA(*p++ & ~MASK30);
  COFF_RAWDATA((char *)p,CODESIZE_BYTES-((char *)p-(char *)(ent->code)));

  return(1);
}
  
#endif
  
/*
 * ic_install_try_me_jmp is given a procedure entry and puts instructions
 * in the code field to do a try_me_else followed by a jump to the clause.
 *
 * wm_try0, wm_try1, wm_try2, and wm_try3 are entry points to the wm_tryX
 * subroutine.
 */

extern	void	wm_try0		PARAMS(( void ));
extern	void	wm_try1		PARAMS(( void ));
extern	void	wm_try2		PARAMS(( void ));
extern	void	wm_try3		PARAMS(( void ));
static Code *trytab[] = {
	(Code *) wm_try0,
	(Code *) wm_try1,
	(Code *) wm_try2,
	(Code *) wm_try3
};

void
ic_install_try_me_jmp(n,clausestart,nextclause)
    ntbl_entry *n;
    long *clausestart;
    long nextclause;
{
    Code delay_instr;

    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    CALL(BLAB(trytab[(n->nargs > NAREGS) ? NAREGS : n->nargs]))
    SUB(TR,imm(16),TR);

#ifndef PACKAGE
    move_const(nextclause,Fail);
#else 	/* PACKAGE */
    SETHI(hi22((long)nextclause),Fail)
    ADD(Fail,imm(lo10((long)nextclause)),Fail)
#endif 	/* PACKAGE */

    delay_instr = *--ic_ptr;
    CALL(BLAB(clausestart))
    ic_put(delay_instr);

    ic_ptr = oldptr;
}

#ifdef PACKAGE

static long trytab_symidx[] = {
        symidx_wm_try0,
        symidx_wm_try1,
        symidx_wm_try2,
        symidx_wm_try3
};

#define try_me_wm_try_OFFSET 0
#define try_me_hi22_OFFSET   8
#define try_me_call_OFFSET   12
#define try_me_lo10_OFFSET   16

static ntbl_entry code_multiple_template;

void
coff_init_code_multiple()
{
  long *cp;

  INSERT_SYM(symidx_wm_try0, 			"_wm_try0");
  INSERT_SYM(symidx_wm_try1, 			"_wm_try1");
  INSERT_SYM(symidx_wm_try2, 			"_wm_try2");
  INSERT_SYM(symidx_wm_try3,		 	"_wm_try3");

  code_multiple_template.nargs = 0;
  ic_install_try_me_jmp(&code_multiple_template,0,0);
  cp = (long *)(code_multiple_template.code);
  CLEAR(cp, try_me_wm_try_OFFSET, ~MASK30);
  CLEAR(cp, try_me_call_OFFSET,   ~MASK30);
}

int
coff_pckg_code_multiple(ent,procname)
	 ntbl_entry *ent;
	 char *procname;
{
  long try_symidx;
  char buf1[64];
  char buf2[64];

  try_symidx = trytab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs];

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(try_symidx),P_R_WDISP30, try_me_wm_try_OFFSET);

  sprintf(buf1,"%s_%x_%s",procname,
		  clauseId(nextClauseAddr(ent->first_clause)),"choice");
  COFF_RELOC_SYMBOL(buf1,P_R_HI22, try_me_hi22_OFFSET);

  sprintf(buf2,"%s_%x_%s",procname,
		  clauseId(ent->first_clause),"code");
  COFF_RELOC_SYMBOL(buf2,P_R_WDISP30, try_me_call_OFFSET);

  COFF_RELOC_SYMBOL(buf1,P_R_LO10, try_me_lo10_OFFSET);

  COFF_RAWDATA(code_multiple_template.code, CODESIZE_BYTES);

  return(1);
}
  
#endif /* PACKAGE */

/*
 * get_target_instruction will get the target instruction to put in a delay
 * slot.  It will also adjust the target.
 */

static Code
get_target_instruction(targp)
    Code **targp;
{
    Code retval;

    retval = **targp;

    if ((retval & 0xc0000000) == 0x40000000 || 	/* Call instruction */
	(retval & 0xc1c00000) == 0x00800000 ||	/* Branch instructions */
	(retval & 0xc1f80000) == 0x81c00000)	/* JMPL instruction */
	return(iSETHI(0,ZERO));		/* return a nop */
    else {
	*targp += 1;			/* advance the target */

	return retval;
    }
}
    
/*
 * ic_install_switch_on_term is used to install the switch on term code in the
 * code field of the given procedure entry point.  If any of straddr, lisaddr,
 * and/or conaddr is equal to var addr then code will be emitted to
 * take these throught the try_me_else code emitted for varaddr.  If any of
 * these are zero, then appropriate code will be emitted for failure.
 *
 * 
 * This sequence will take at most 27 words.
 */

void
ic_install_switch_on_term(n,varaddr,straddr,lisaddr,conaddr,emask)
    ntbl_entry *n;
    Code *varaddr, *straddr, *lisaddr, *conaddr;
    int emask;
{
    LABEL deref, ground;		/* our labels */
    LABEL lis,str,con;
    Code fillinstr;
    int displ;

    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    lis = str = con = (LABEL) 0;

    ic_install_emask(emask);

	ANDcc(A1,imm(0x03),tmp1)
    BLABDCL(deref)
	BNE(FLAB(ground))
	ANDN(A1,imm(0x03),S)
	LD(S,ZERO,A1)
	CMP(S,A1)
	BNE(BLAB(deref))
	ANDcc(A1,imm(0x03),tmp1)

	fillinstr = get_target_instruction(&varaddr);
	CALL(BLAB(varaddr));
	ic_put(fillinstr);

    FLABDCL(ground)
	CMP(tmp1,imm(MTP_LIST))
	if (lisaddr) {
	    fillinstr = get_target_instruction(&lisaddr);
	    displ = BLAB(lisaddr);
	    if (is22BitDispl(displ))
		BE_a(displ)
	    else
		BE_a(FLAB(lis))
	    ic_put(fillinstr);
	}
	if (straddr) {
	    fillinstr = get_target_instruction(&straddr);
	    displ = BLAB(straddr);
	    if (is22BitDispl(displ))
		BCS_a(displ) 		/* branch on less than */
	    else
		BCS_a(FLAB(str))
	    ic_put(fillinstr);
	}
	if (conaddr) {
	    fillinstr = get_target_instruction(&conaddr);
	    displ = BLAB(conaddr);
	    if (is22BitDispl(displ))
		BGU_a(displ)
	    else
		BGU_a(FLAB(con))
	    ic_put(fillinstr);
	}
	if (!conaddr || !straddr || !lisaddr) {
	    JMPL(Fail,ZERO,ZERO)
	    NOP
	}

	if (lis) {
	    FLABDCL(lis)
		fillinstr = get_target_instruction(&lisaddr);
		CALL(BLAB(lisaddr))
		ic_put(fillinstr);
	}
	if (str) {
	    FLABDCL(str)
		fillinstr = get_target_instruction(&straddr);
		CALL(BLAB(straddr))
		ic_put(fillinstr);
	}
	if (con) {
	    FLABDCL(con)
		fillinstr = get_target_instruction(&conaddr);
		CALL(BLAB(conaddr))
		ic_put(fillinstr);
	}
    
    ic_ptr = oldptr;
}



/*
 * ic_install_builtin is used to install a builtin.
 */

extern	void	wm_exec_builtin0	PARAMS(( void ));
extern	void	wm_exec_builtin1	PARAMS(( void ));
extern	void	wm_exec_builtin2	PARAMS(( void ));
extern	void	wm_exec_builtin3	PARAMS(( void ));

static Code *ebtab[] = {
    (Code *) wm_exec_builtin0,
    (Code *) wm_exec_builtin1,
    (Code *) wm_exec_builtin2,
    (Code *) wm_exec_builtin3
};

void
ic_install_builtin(n,builtin)
    ntbl_entry *n;
    int (*builtin) PARAMS(( void ));
{
    Code delay_instr;

    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

#ifndef PACKAGE
    move_const(((long) builtin), UArg1);
#else 	/* PACKAGE */
    SETHI(hi22((long)builtin),UArg1)
    ADD(UArg1,imm(lo10((long)builtin)),UArg1)
#endif 	/* PACKAGE */
    delay_instr = *--ic_ptr;

    CALL(BLAB(ebtab[(n->nargs > NAREGS) ? NAREGS : n->nargs]))
    ic_put(delay_instr);

    ic_ptr = oldptr;
}

#ifdef PACKAGE

#define builtin_hi22_OFFSET   0
#define builtin_call_OFFSET   4
#define builtin_lo10_OFFSET   8

static ntbl_entry code_builtin_template;

static long ebtab_symidx[] = {
        symidx_wm_exec_builtin0,
        symidx_wm_exec_builtin1,
        symidx_wm_exec_builtin2,
        symidx_wm_exec_builtin3
};

int
coff_init_code_builtin()
{
  INSERT_SYM(symidx_wm_exec_builtin0,	"_wm_exec_builtin0");
  INSERT_SYM(symidx_wm_exec_builtin1,	"_wm_exec_builtin1");
  INSERT_SYM(symidx_wm_exec_builtin2,	"_wm_exec_builtin2");
  INSERT_SYM(symidx_wm_exec_builtin3,	"_wm_exec_builtin3");

  code_builtin_template.nargs = 0;

  ic_install_builtin(&code_builtin_template,0);

  CLEAR(code_builtin_template.code, builtin_call_OFFSET, ~MASK30);
}


int
coff_pckg_code_builtin(ent)
	 ntbl_entry *ent;
{
  long bltaddr;
  char *bltname, *cp;
  long eb_symidx;

  eb_symidx = ebtab_symidx[(ent->nargs > NAREGS) ? NAREGS : ent->nargs];

  cp = (char *)(ent->code);
  bltaddr =  (*(long *)(cp + builtin_hi22_OFFSET) & MASK22) << 10;
  bltaddr |= *(long *)(cp+builtin_lo10_OFFSET) & MASK10;
  bltname = (char *) builtin_name(bltaddr);

  COFF_RELOC_SYMBOL(bltname,P_R_HI22,builtin_hi22_OFFSET);
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(eb_symidx),P_R_WDISP30,
						builtin_call_OFFSET);
  COFF_RELOC_SYMBOL(bltname,P_R_LO10,builtin_lo10_OFFSET);

  COFF_RAWDATA(code_builtin_template.code,CODESIZE_BYTES);

  return(1);
}

#endif  /* PACKAGE */

/*
 * ic_install_true is used to install the builtin true.
 */

void
ic_install_true(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    JMPL(CP,ZERO,ZERO)
    MOVE(OldE,E)

    ic_ptr = oldptr;
}


/*
 * ic_install_fail is used to install code in a procedure entry which fails.
 * This is useful for establishing a defined procedure with no clauses.
 */

void
ic_install_fail(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
	ic_ptr = n->code;

    JMPL(Fail,ZERO,ZERO)
    NOP

    ic_ptr = oldptr;
}


#ifdef PACKAGE

int
coff_pckg_code_true(ent)
	 ntbl_entry *ent;
{
  COFF_RAWDATA(ent->code, CODESIZE_BYTES);

  return(1);
}

#endif /* PACKAGE */

/*
 * ic_install_equal is called to install =/2 in the procedure entry table
 */

void
ic_install_equal(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    MOVE(A1,UArg1)
    MOVE(A2,UArg2)
    MOVE(OldE,E)
    CALL(BLAB(wm_unify))
    SUB(CP,imm(8),RET)		/* Set up RET properly */

    ic_ptr = oldptr;
}

#ifdef PACKAGE

#define wm_unify_OFFSET 12

static ntbl_entry code_equal_template;

void
coff_init_code_equal()
{
  /* INSERT_SYM(symidx_wm_unify, "_wm_unify"); - see coff_init_g_value() */
  
  ic_install_equal(&code_equal_template);

  CLEAR(code_equal_template.code, wm_unify_OFFSET, ~MASK30);
}

int
coff_pckg_code_equal(ent)
	 ntbl_entry *ent;
{
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_unify),
						P_R_WDISP30,wm_unify_OFFSET);
  COFF_RAWDATA(code_equal_template.code, CODESIZE_BYTES);

  return(1);
}

#endif /* PACKAGE */

/*
 * ic_install_call is used to install call/1 and other procedures which
 *	need the module id of the caller.
 */


void
ic_install_call(n,whereto)
    ntbl_entry *n;
    Code *whereto;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    CALL(2)			/* skip around the delay instruction */
    SUB(RET,imm((int)(char *)(((ntbl_entry *) 0)->code)),T1)
    LDUH(T1,imm((int)(char *)&(((ntbl_entry *) 0)->modid)),T1)
    SLL(T1,imm(MTP_CONSTSHIFT),T1)
    CALL(BLAB(whereto))
    ADD(T1,imm(MTP_SYM),T1)

    ic_ptr = oldptr;
}

#ifdef PACKAGE

#define whereto_OFFSET 16

static ntbl_entry code_call_template;

void
coff_init_code_call()
{
  ic_install_call(&code_call_template,0);

  CLEAR(code_call_template.code, whereto_OFFSET, ~MASK30);
}

int
coff_pckg_code_call(ent)
	 ntbl_entry *ent;
{
  Code *paddr;
  long offs;
  char *cp;

  cp = (char *)(ent->code);
  offs = *(long *)(cp+whereto_OFFSET) & MASK30;
  if (offs & 0x20000000)
	offs |= ~MASK30;
  paddr = (Code *) ((long *)(cp+whereto_OFFSET) + offs);

  COFF_RELOC_SYMBOL(builtin_name(paddr),P_R_WDISP30,whereto_OFFSET);
  COFF_RAWDATA(code_call_template.code, CODESIZE_BYTES);

  return(1);
}

#endif /* PACKAGE */


/*
 * ic_install_module_closure installs code which gets the module id (an
 * integer) of the current procedure and installs this integer as the first
 * argument.
 */


void
ic_install_module_closure(n,whereto)
    ntbl_entry *n;
    Code *whereto;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;

    CALL(2)			/* jump past delay instruction */
    SUB(RET,imm((int)(char *)(((ntbl_entry *) 0)->code)),T1)

    /*
     * Shift arguments as needed
     */
    
    if (n->nargs >= 3) {
	ST(A3, E, imm(16))
    }

    if (n->nargs >= 2) {
	MOVE(A2,A3)
    }

    if (n->nargs >= 1) {
	MOVE(A1,A2)
    }

    /*
     * Load module id into A1 (untagged)
     */
    
    LDUH(T1,imm((int)(char *)&(((ntbl_entry *) 0)->modid)),A1)

    /*
     * Increase the size of the frame by one word
     */
    
    SUB(SP,imm(4),SP)
    MOVE(SP,E)

    /*
     * Tag the module id which is in A1
     */
    
    SLL(A1,imm(4),A1)

    /*
     * Jump to the other place to go and finish tagging A1
     */
    
    CALL(BLAB(whereto))
    ADD(A1,imm(MTP_SYM),A1)

    ic_ptr = oldptr;

}

#ifdef PACKAGE

void
coff_pckg_code_modclosure(ent)
	 ntbl_entry *ent;
{
  unsigned long *p;
  char buf[64];
  Code *paddr;
  long offs;
  ntbl_entry *toent;

  p = (unsigned long *) (ent->code);
  COFF_LONG_RAWDATA(*p++);
  COFF_LONG_RAWDATA(*p++);

  if (ent->nargs >= 3) {
	COFF_LONG_RAWDATA(*p++);
  }
  if (ent->nargs >= 2) {
	COFF_LONG_RAWDATA(*p++);
  }				
  if (ent->nargs >= 1) {
	COFF_LONG_RAWDATA(*p++);
  }

  COFF_RAWDATA(p, 4 * sizeof(long));
  p += 4;
  offs = (long)(*p & MASK30);
  if (offs & 0x20000000)
	offs |= ~MASK30;
  paddr = (Code *) (p + offs);
  toent = (ntbl_entry *)(paddr - NTBL_ENTRYSIZE);
  ntbl_entry_name(toent,"code",buf);
  COFF_RELOC_SYMBOL(buf,P_R_WDISP30,0);
  COFF_LONG_RAWDATA(*p++ & ~MASK30);
  COFF_RAWDATA(p,CODESIZE_BYTES-((char *)p-(char*)(ent->code)));

  return(1);
}

#endif /* PACKAGE */

/*
 * ic_install_next_choice_in_a_deleted_clause will do what it says, fix up
 * the choice point instruction in a deleted clause so that should it be
 * run again, the system will end up going to a clause in the active choice
 * point chain.
 */

extern	void	wm_nciadc	PARAMS(( void ));

void
ic_install_next_choice_in_a_deleted_clause(buf)
    Code *buf;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    CALL(BLAB(wm_nciadc))
    MOVE(RET,UArg1)

    ic_ptr = oldptr;
}

/*
 * ic_install_try_me (not used )
 *
 */

void
ic_install_try_me(ic,nextclause,nargs)
    Code *ic;
    PWord nextclause;
    int nargs;
{
    Code *oldptr = ic_ptr;
	register ic_uptr_type ic_uptr;
	ic_uptr.code_ptr = ic;

    CALL(BLAB(trytab[(nargs > NAREGS) ? NAREGS : nargs]))
    SUB(TR,imm(16),TR)

    SETHI(hi22(nextclause),Fail)
    ADD(Fail,imm(lo10(nextclause)),Fail)

    ic_ptr = oldptr;
}


/*
 * ic_install_retry_me
 *
 */

extern	void	wm_retry_u0	PARAMS(( void ));
extern	void	wm_retry_u1	PARAMS(( void ));
extern	void	wm_retry_u2	PARAMS(( void ));
extern	void	wm_retry_u3	PARAMS(( void ));
static Code *retry_u_tab[] = {
    (Code *) wm_retry_u0,
    (Code *) wm_retry_u1,
    (Code *) wm_retry_u2,
    (Code *) wm_retry_u3
};

extern	long	wm_retry0	PARAMS(( void ));
extern	void	wm_retry1	PARAMS(( void ));
extern	void	wm_retry2	PARAMS(( void ));
extern	void	wm_retry3	PARAMS(( void ));
static Code *retry_tab[] = {
    (Code *) wm_retry0,
    (Code *) wm_retry1,
    (Code *) wm_retry2,
    (Code *) wm_retry3
};

void
ic_install_retry_me(buf,nextclause,nargs,emask)
    Code *buf;
    PWord nextclause;
    int nargs;
    int emask;
{
    int displ;

	Code *oldptr = ic_ptr;
	ic_ptr = buf;

    if (nargs > NAREGS)
	nargs = NAREGS;
    
    if (emask & EMSK_CP)
	displ = retry_tab[nargs] - ic_ptr;
    else
	displ = retry_u_tab[nargs] - ic_ptr;
    
    CALL(displ)
    MOVE(SPB,E)

    SETHI(hi22(nextclause),Fail)
    ADD(Fail,imm(lo10(nextclause)),Fail)

	ic_ptr=oldptr;
}

#ifdef PACKAGE

static long retry_tab_symidx[] = {
        symidx_wm_retry0,
        symidx_wm_retry1,
        symidx_wm_retry2,
        symidx_wm_retry3
};

static long retry_u_tab_symidx[] = {
        symidx_wm_retry_u0,
        symidx_wm_retry_u1,
        symidx_wm_retry_u2,
        symidx_wm_retry_u3
};

#define CHOICESIZE        (WCI_CLAUSECODE - WCI_CHOICECODE)
#define CHOICESIZE_BYTES  (CHOICESIZE * sizeof(long))

static long retry_me_template[CHOICESIZE];

#define retry_me_call_OFFSET   0
#define retry_me_hi22_OFFSET   8
#define retry_me_lo10_OFFSET   12


coff_init_retry_me()
{
  INSERT_SYM(symidx_wm_retry0, 		"_wm_retry0");
  INSERT_SYM(symidx_wm_retry1, 		"_wm_retry1");
  INSERT_SYM(symidx_wm_retry2, 		"_wm_retry2");
  INSERT_SYM(symidx_wm_retry3, 		"_wm_retry3");
  INSERT_SYM(symidx_wm_retry_u0, 		"_wm_retry_u0");
  INSERT_SYM(symidx_wm_retry_u1, 		"_wm_retry_u1");
  INSERT_SYM(symidx_wm_retry_u2, 		"_wm_retry_u2");
  INSERT_SYM(symidx_wm_retry_u3, 		"_wm_retry_u3");

  ic_install_retry_me(retry_me_template,0,0,0);

  CLEAR(retry_me_template, retry_me_call_OFFSET, ~MASK30);
}

coff_pckg_retry_me(procname,ca,ent)
	 char *procname;
	 long *ca;
	 ntbl_entry *ent;
{
  int  n;
  unsigned long *p;
  long retry_symidx;
  char buf[64];

  n = (ent->nargs > NAREGS) ? NAREGS : ent->nargs;
  if (emaskCode(ca) & EMSK_CP)
	retry_symidx =  retry_tab_symidx[n];
  else
	retry_symidx =  retry_u_tab_symidx[n];

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(retry_symidx),
						P_R_WDISP30,retry_me_call_OFFSET);

  sprintf(buf,"%s_%x_%s",
		  procname,clauseId(nextClauseAddr(ca)),"choice");
  COFF_RELOC_SYMBOL(buf,P_R_HI22,retry_me_hi22_OFFSET);
  COFF_RELOC_SYMBOL(buf,P_R_LO10,retry_me_lo10_OFFSET);

  COFF_RAWDATA(retry_me_template, CHOICESIZE_BYTES);

  return(1);
}

#endif /* PACKAGE */

/*
 * ic_install_trust_me
 *
 */

extern	void	wm_trust_u0	PARAMS(( void ));
extern	void	wm_trust_u1	PARAMS(( void ));
extern	void	wm_trust_u2	PARAMS(( void ));
extern	void	wm_trust_u3	PARAMS(( void ));
extern	void	wm_trust0	PARAMS(( void ));
extern	void	wm_trust1	PARAMS(( void ));
extern	void	wm_trust2	PARAMS(( void ));
extern	void	wm_trust3	PARAMS(( void ));

static Code *trust_u_tab[] = {
    (Code *) wm_trust_u0,
    (Code *) wm_trust_u1,
    (Code *) wm_trust_u2,
    (Code *) wm_trust_u3
};

static Code *trust_tab[] = {
    (Code *) wm_trust0,
    (Code *) wm_trust1,
    (Code *) wm_trust2,
    (Code *) wm_trust3
};

void
ic_install_trust_me(buf, clausestart, nargs, emask)
    Code *buf;
    PWord clausestart;
    int nargs, emask;
{
    int displ;

    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    if (nargs > NAREGS) nargs = NAREGS;
    if (emask & EMSK_CP)
	displ = trust_tab[nargs] - ic_ptr;
    else
	displ = trust_u_tab[nargs] - ic_ptr;
    
    CALL(displ)
    MOVE(SPB,E)
    BA(BLAB(clausestart))
    MOVE(E,SP)

	ic_ptr = oldptr;
}


#ifdef PACKAGE

static long trust_tab_symidx[] = {
        symidx_wm_trust0,
        symidx_wm_trust1,
        symidx_wm_trust2,
        symidx_wm_trust3
};

static long trust_u_tab_symidx[] = {
        symidx_wm_trust_u0,
        symidx_wm_trust_u1,
        symidx_wm_trust_u2,
        symidx_wm_trust_u3
};


coff_init_trust_me()
{
  INSERT_SYM(symidx_wm_trust0, 		"_wm_trust0");
  INSERT_SYM(symidx_wm_trust1,	 	"_wm_trust1");
  INSERT_SYM(symidx_wm_trust2, 		"_wm_trust2");
  INSERT_SYM(symidx_wm_trust3, 		"_wm_trust3");
  INSERT_SYM(symidx_wm_trust_u0, 		"_wm_trust_u0");
  INSERT_SYM(symidx_wm_trust_u1, 		"_wm_trust_u1");
  INSERT_SYM(symidx_wm_trust_u2, 		"_wm_trust_u2");
  INSERT_SYM(symidx_wm_trust_u3, 		"_wm_trust_u3");
}

int
coff_pckg_trust_me(ca,ent)
	 long *ca;
	 ntbl_entry *ent;
{
  unsigned long *p;
  int  n;
  long trust_symidx;

  p = (unsigned long *) (choiceEntry(ca));
  n = (ent->nargs > NAREGS) ? NAREGS : ent->nargs;
  if (emaskCode(ca) & EMSK_CP)
	trust_symidx =  trust_tab_symidx[n];
  else
	trust_symidx =  trust_u_tab_symidx[n];

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(trust_symidx),P_R_WDISP30,0);

  COFF_LONG_RAWDATA(*p++ & ~MASK30);
  COFF_RAWDATA(p, CHOICESIZE_BYTES-4);

  return(1);
}

#endif /* PACKAGE */

/*
 * ic_install_no installs both the no part for queries and the little
 * piece of code from which execution will start from.  The address of
 * the place to start is returned as the value from ic_install_no.
 */

Code *
ic_install_no(buf,clausestart,nocatcher)
    Code *buf;
    Code *clausestart;
    char *nocatcher;		/* name of the no catcher */
{
    Code *startaddr;

    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    /*
     * Install a retry_me for the no catcher
     */
    
    CALL(BLAB(wm_retry_u0))
    MOVE(SPB,E)
    move_const((long) buf,Fail);

    /*
     * Run the no catcher
     */
    
    CALL(BLAB(w_nameentry(MODULE_BUILTINS,find_token(nocatcher),0)->exec_entry))
    NOP

    /*
     * This is where the query actually starts, so set startaddr.
     */
    
    startaddr = ic_ptr;

    /*
     * Allocate space on the stack for the CP and OldE
     */
    
    SUB(SP,imm(8),SP)
    MOVE(SP,E)

    /*
     * Create a choice point with no arguments
     */
    
    OR(SPB,imm(1),SPB)			/* for compaction purposes */
    CALL(BLAB(wm_try0))
    SUB(TR,imm(16),TR)
    move_const((long) buf,Fail);

    /*
     * Allocate another space on the stack for CP and OldE
     */
    
    SUB(SP,imm(8),SP)
    MOVE(SP,E)

    /*
     * Create another choice point which could be cut away
     */
    
    CALL(BLAB(wm_try0))
    SUB(TR,imm(16),TR)
    move_const((long) buf,Fail);

    /*
     * Branch to the start of the query or command
     */
    
    BA_a(BLAB(clausestart))
    NOP

    ic_ptr = oldptr;
    return startaddr;
}

/*
 * ic_install_reference is called by resolve_reference to install a jump to
 * a non-builtin.
 */

void
ic_install_reference(buf, where)
    Code *buf;
    PWord where;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    CALL(BLAB(where))
    NOP

	ic_ptr = oldptr;
}

/*
 * ic_install_try
 *
 *	This procedure will install a try sequence for the indexer.
 *
 *	ic_ptr	is the address to start installing the try sequence at
 *	cstart	is the address to jump to after the choice point is created
 *	nargs	is the number of arguments in the clause
 *
 *	The next free address is returned as the value of the function.
 *
 * 	Note:	The name ic_ptr was chosen as the name for the first argument
 *		because it is used by the macros which lay down code sequences
 *
 */

long *
ic_install_try(ic, cstart, nargs)
    Code *ic;
    Code *cstart;
    int nargs;
{
    Code *tmp_ic_ptr;
    Code *oldptr = ic_ptr;
	register ic_uptr_type ic_uptr;
	ic_uptr.code_ptr = ic;

    CALL(BLAB(trytab[(nargs > NAREGS) ? NAREGS : nargs]))
    SUB(TR,imm(16),TR)
    CALL(BLAB(cstart))
    ADD(RET,imm(8),Fail)

    tmp_ic_ptr = ic_ptr;
    ic_ptr = oldptr;
    return (long *) tmp_ic_ptr;
}

/*
 * ic_install_retry
 *
 *	This function is called by the indexer to install a retry sequence.
 *
 *	ic_ptr	is the address to start installing the retry at
 *	cstart	is the pointer to the place to jump to after performing the
 *		retry operation.
 *	nargs	is the number of arguments in the procedure.
 *	emask	is the mask from the clause indicating whether or not the
 *		cp and ce need to be restored.  (Not applicable on all
 *		machines)
 *
 *	The next free location is returned as the result of this function.
 *	Since the indexer needs to maintain longword alignment, this code
 *	has been carefully constructed to omaintain longword alignment.
 *
 *	Note:	The name ic_ptr was chosen as the name for the first argument
 *		because it is used by the macros which lay down code sequences
 */

long *
ic_install_retry(ic, cstart, nargs, emask)
    Code *ic;
    Code *cstart;
    int nargs;
    int emask;
{
    int displ;

    Code *tmp_ic_ptr;
    Code *oldptr = ic_ptr;
    ic_ptr = (Code *) ic;

    if (nargs > NAREGS)
	nargs = NAREGS;
    
    if (emask & EMSK_CP)
	displ = retry_tab[nargs] - ic_ptr;
    else
	displ = retry_u_tab[nargs] - ic_ptr;
    
    CALL(displ)
    MOVE(SPB,E)
    CALL(BLAB(cstart))
    ADD(RET,imm(8),Fail)

	tmp_ic_ptr = ic_ptr;
	ic_ptr = oldptr;
    return (long *) tmp_ic_ptr;
}

/*
 * ic_install_trust
 *
 *	Installs a trust sequence for the indexer.
 *
 *	ic_ptr	is the address to start installing the trust sequence at
 *	cstart	is a pointer to the address to jump to
 *	nargs	is the number of arguments for the procedure of which
 *		the clause is a part
 *	emask	is the mask indicating whether the continuation pointer
 *		needs to be restored or not
 *
 *	The next free address to continue installation at will be returned
 *	as the value of this function.  The indexer expects longword alignment
 *	to be maintained on this return value.  The code has been constructed
 *	to facilitate this.
 */

long *
ic_install_trust(ic, cstart, nargs, emask)
    register Code *ic;
    Code *cstart;
    int nargs;
    int emask;
{
    int displ;

    Code *oldptr = ic_ptr;
    Code *tmp_ic_ptr;
	register ic_uptr_type ic_uptr;
	ic_uptr.code_ptr = ic;

    if (nargs > NAREGS) nargs = NAREGS;
    if (emask & EMSK_CP)
	displ = trust_tab[nargs] - ic_ptr;
    else
	displ = trust_u_tab[nargs] - ic_ptr;
    
    CALL(displ)
    MOVE(SPB,E)
    CALL(BLAB(cstart))
    MOVE(E,SP)

    tmp_ic_ptr = ic_ptr;
    ic_ptr = oldptr;
    return (long *) tmp_ic_ptr;
}

/*
 * ic_install_tree_overhead
 *
 *	Installs switch_on_constant or switch_on_structure instructions for
 *	the indexer.
 *
 *	swaddr		is the address of the switch_on subroutine
 *	nentries	are the number of entries which we are concerned with
 *	ic_ptr		is the place to start installing the sequence at
 *
 *	Note that ic_ptr is also a global variable which is usually referenced
 *	by the ic_put macros.  In this case, however, it referes to the
 *	parameter.
 *
 *	The following code is emitted for the SPARC
 *
 *	call	swaddr
 *	sethi	nentries, T1
 *
 *	wm_sw_const and wm_sw_struct are expected to use RET to find the
 *	table.  The table will be at 8 plus the value of RET.  T1 will
 *	need to be shifted right in order to make use of the number of
 *	entries field.  The sethi instruction was a convienient way to
 *	get a fairly large data item into a register.  We would have been
 *	stuck with 13 bits otherwise.
 */

Code *
ic_install_tree_overhead(swaddr, nentries, ic)
    long *swaddr;
    int nentries;
    Code *ic;
{
    Code *tmp_ic_ptr;
    Code *oldptr = ic_ptr;
	register ic_uptr_type ic_uptr;
	ic_uptr.code_ptr = ic;

    CALL(BLAB(swaddr))
    SETHI(nentries,T1)

    tmp_ic_ptr = ic_ptr;
    ic_ptr = oldptr;
    return tmp_ic_ptr;
}



#ifdef PACKAGE

void
coff_init_icode2()
{
  coff_init_overflow();
  coff_init_code_builtin();
  coff_init_code_call();
  coff_init_code_equal();
  coff_init_libbreak();
  coff_init_resolve_ref();
  coff_init_code_multiple();
  coff_init_retry_me();
  coff_init_trust_me();
}

#endif /* PACKAGE */
