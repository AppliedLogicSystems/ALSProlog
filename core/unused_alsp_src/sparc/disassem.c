/*
 * disassem.c			-- disassembler for the SPARC
 *	Copyright (c) 1990-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/22/90
 * Revision History:
 */

#include "defs.h"
#include "wintcode.h"

#ifdef TESTDISASSEM
#define PI_printf printf
main()
{
    list_asm(main,100);
}
#endif

struct iinfo {
	char *name;
	int format;
};

static	void	print_instruction	PARAMS(( void ));
static	int	offset			PARAMS(( int, int ));
static	char *	symbolic_name		PARAMS(( long * ));
static	void	print_format1_instruction PARAMS(( void ));
static	void	print_format2_instruction PARAMS(( void ));
static	void	print_format3_instruction PARAMS(( struct iinfo * ));
static	void	print_conditional_instr	PARAMS(( char *, char **, long ));
static	void	print_unimplemented	PARAMS(( long ));
static	char *	getaddress		PARAMS(( long ));
static	char *	reg_or_imm		PARAMS(( long ));

static Code *ip;

void
list_asm(addr,n)
    Code *addr;
    int n;
{
    long *stopaddr = addr+n;
    ip = addr;
    while (ip < stopaddr) {
	PI_printf("%x:\t",(int) ip);
	print_instruction();
    }
}

#ifdef notdef
static char *regs[] = {
    "%0",			/* r0 */
    "%1",			/* r1 */
    "%2",			/* r2 */
    "%3",			/* r3 */
    "%4",			/* r4 */
    "%5",			/* r5 */
    "%6",			/* r6 */
    "%7",			/* r7 */
    "%8",			/* r8 */
    "%9",			/* r9 */
    "%10",			/* r10 */
    "%11",			/* r11 */
    "%12",			/* r12 */
    "%13",			/* r13 */
    "%14",			/* r14 */
    "%15",			/* r15 */
    "%16",			/* r16 */
    "%17",			/* r17 */
    "%18",			/* r18 */
    "%19",			/* r19 */
    "%20",			/* r20 */
    "%21",			/* r21 */
    "%22",			/* r22 */
    "%23",			/* r23 */
    "%24",			/* r24 */
    "%25",			/* r25 */
    "%26",			/* r26 */
    "%27",			/* r27 */
    "%28",			/* r28 */
    "%29",			/* r29 */
    "%30",			/* r30 */
    "%31"			/* r31 */
};
#endif

static char *regs[] = {
    "ZERO",			/* r0 */
    "UArg1",			/* r1 */
    "UArg2",			/* r2 */
    "tmp1",			/* r3 */
    "tmp2",			/* r4 */
    "%5",			/* r5 */
    "%6",			/* r6 */
    "%7",			/* r7 */
    "H",			/* r8 */
    "HB",			/* r9 */
    "B",			/* r10 */
    "TR",			/* r11 */
    "SPB",			/* r12 */
    "E",			/* r13 */
    "cSP",			/* r14 */
    "RET",			/* r15 */
    "HeapBase",			/* r16 */
    "StkBot",			/* r17 */
    "OldE",			/* r18 */
    "CP",			/* r19 */
    "Fail",			/* r20 */
    "Safety",			/* r21 */
    "SP",			/* r22 */
    "S",			/* r23 */
    "A1",			/* r24 */
    "A2",			/* r25 */
    "A3",			/* r26 */
    "T1",			/* r27 */
    "T2",			/* r28 */
    "T3",			/* r29 */
    "cFP",			/* r30 */
    "cRET"			/* r31 */
};

static char unimplemented[] = "unimplemented";

/*
 * Operand formats for op3 instructions.
 */

#define OF_UNIMP	0		/* for unimplemented opcodes */
#define	OF_a_rd		1		/* [address], regd */
#define OF_ra_rd	2		/* [regaddr] asi, regd */
#define OF_a_freg	3		/* [address], freg */
#define OF_a_fsr	4		/* [address], %fsr */
#define OF_a_creg	5		/* [address], creg */
#define OF_a_csr	6		/* [address], %csr */
#define OF_rd_a		7		/* regd, [address] */
#define OF_rd_ra	8		/* regd, [regaddr] asi */
#define OF_freg_a	9		/* freg, [address] */
#define OF_fsr_a	10		/* %fsr, [address] */
#define OF_fq_a		11		/* %fq, [address] */
#define OF_creg_a	12		/* creg, [address] */
#define OF_csr_a	13		/* %csr, [address] */
#define OF_cq_a		14		/* %cq, [address] */
#define OF_r_ri_rd	15		/* reg, reg_or_imm, regd */
#define OF_a		16		/* [address]	*/
#define OF_y_rd		17		/* %y, regd */
#define OF_psr_rd	18		/* %psr, regd */
#define OF_wim_rd	19		/* %wim, regd */
#define OF_tbr_rd	20		/* %tbr, regd */
#define OF_r_ri_y	21		/* reg, reg_or_imm, %y */
#define OF_r_ri_psr	22		/* reg, reg_or_imm, %psr */
#define OF_r_ri_wim	23		/* reg, reg_or_imm, %wim */
#define OF_r_ri_tbr	24		/* reg, reg_or_imm, %tbr */
#define OF_ticc		25		/* ticc instruction */
#define OF_fpop1	26		/* fpop instruction */
#define OF_fpop2	27		/* fpop instruction */
#define OF_cpop1	28		/* cpop instruction */
#define OF_cpop2	29		/* cpop instruction */


static struct iinfo op3_10[] = {
    { "add", OF_r_ri_rd },	/* 000000 */
    { "and", OF_r_ri_rd },	/* 000001 */
    { "or", OF_r_ri_rd },	/* 000010 */
    { "xor", OF_r_ri_rd },	/* 000011 */
    { "sub", OF_r_ri_rd },	/* 000100 */
    { "andn", OF_r_ri_rd },	/* 000101 */
    { "orn", OF_r_ri_rd },	/* 000110 */
    { "xnor", OF_r_ri_rd },	/* 000111 */
    { "addx", OF_r_ri_rd },	/* 001000 */
    { unimplemented, OF_UNIMP},	/* 001001 */
    { unimplemented, OF_UNIMP},	/* 001010 */
    { unimplemented, OF_UNIMP},	/* 001011 */
    { "subx", OF_r_ri_rd },	/* 001100 */
    { unimplemented, OF_UNIMP},	/* 001101 */
    { unimplemented, OF_UNIMP},	/* 001110 */
    { unimplemented, OF_UNIMP},	/* 001111 */
    { "addcc", OF_r_ri_rd },	/* 010000 */
    { "andcc", OF_r_ri_rd },	/* 010001 */
    { "orcc", OF_r_ri_rd },	/* 010010 */
    { "xorcc", OF_r_ri_rd },	/* 010011 */
    { "subcc", OF_r_ri_rd },	/* 010100 */
    { "andncc", OF_r_ri_rd },	/* 010101 */
    { "orncc", OF_r_ri_rd },	/* 010110 */
    { "xnorcc", OF_r_ri_rd },	/* 010111 */
    { "addxcc", OF_r_ri_rd },	/* 011000 */
    { unimplemented, OF_UNIMP},	/* 011001 */
    { unimplemented, OF_UNIMP},	/* 011010 */
    { unimplemented, OF_UNIMP},	/* 011011 */
    { "subxcc", OF_r_ri_rd },	/* 011100 */
    { unimplemented, OF_UNIMP},	/* 011101 */
    { unimplemented, OF_UNIMP},	/* 011110 */
    { unimplemented, OF_UNIMP},	/* 011111 */
    { "taddcc", OF_r_ri_rd },	/* 100000 */
    { "tsubcc", OF_r_ri_rd },	/* 100001 */
    { "taddcctv", OF_r_ri_rd },	/* 100010 */
    { "tsubcctv", OF_r_ri_rd },	/* 100011 */
    { "mulscc", OF_r_ri_rd },	/* 100100 */
    { "sll", OF_r_ri_rd },	/* 100101 */
    { "srl", OF_r_ri_rd },	/* 100110 */
    { "sra", OF_r_ri_rd },	/* 100111 */
    { "rd", OF_y_rd },		/* 101000 (RDY) */
    { "rd", OF_psr_rd },	/* 101001 (RDPSR) */
    { "rd", OF_wim_rd }, 	/* 101010 (RDWIM) */
    { "rd", OF_tbr_rd },	/* 101011 (RDTBR) */
    { unimplemented, OF_UNIMP},	/* 101100 */
    { unimplemented, OF_UNIMP},	/* 101101 */
    { unimplemented, OF_UNIMP},	/* 101110 */
    { unimplemented, OF_UNIMP},	/* 101111 */
    { "wr", OF_r_ri_y },	/* 110000 (WRY) */
    { "wr", OF_r_ri_psr },	/* 110001 (WRPSR) */
    { "wr", OF_r_ri_wim },	/* 110010 (WRWIM) */
    { "wr", OF_r_ri_wim },	/* 110011 (WRTBR) */
    { "fpop1", OF_fpop1 },	/* 110100 */
    { "fpop2", OF_fpop2 },	/* 110101 */
    { "cpop1", OF_cpop1 },	/* 110110 */
    { "cpop2", OF_cpop2 },	/* 110111 */
    { "jmpl", OF_a_rd },	/* 111000 */
    { "rett", OF_a },		/* 111001 */
    { "ticc", OF_ticc },	/* 111010 */
    { "iflush", OF_a },		/* 111011 */
    { "save", OF_r_ri_rd },	/* 111100 */
    { "restore", OF_r_ri_rd },	/* 111101 */
    { unimplemented, OF_UNIMP},	/* 111110 */
    { unimplemented, OF_UNIMP},	/* 111111 */
};


static struct iinfo op3_11[] = {
    { "ld", OF_a_rd },		/* 000000 */
    { "ldub", OF_a_rd },	/* 000001 */
    { "lduh", OF_a_rd },	/* 000010 */
    { "ldd", OF_a_rd },		/* 000011 */
    { "st", OF_rd_a },		/* 000100 */
    { "stb", OF_rd_a },		/* 000101 */
    { "sth", OF_rd_a },		/* 000110 */
    { "std", OF_rd_a },		/* 000111 */
    { unimplemented, OF_UNIMP},	/* 001000 */
    { "ldsb", OF_a_rd },	/* 001001 */
    { "ldsh", OF_a_rd },	/* 001010 */
    { unimplemented, OF_UNIMP},	/* 001011 */
    { unimplemented, OF_UNIMP},	/* 001100 */
    { "ldstub", OF_a_rd },	/* 001101 */
    { unimplemented, OF_UNIMP},	/* 001110 */
    { "swap", OF_a_rd },	/* 001111 */
    { "lda", OF_ra_rd },	/* 010000 */
    { "lduba", OF_ra_rd },	/* 010001 */
    { "lduha", OF_ra_rd },	/* 010010 */
    { "ldda", OF_ra_rd },	/* 010011 */
    { "sta", OF_rd_ra },	/* 010100 */
    { "stba", OF_rd_ra },	/* 010101 */
    { "stha", OF_rd_ra },	/* 010110 */
    { "stda", OF_rd_ra },	/* 010111 */
    { unimplemented, OF_UNIMP},	/* 011000 */
    { "ldsba", OF_ra_rd },	/* 011001 */
    { "ldsha", OF_ra_rd },	/* 011010 */
    { unimplemented, OF_UNIMP},	/* 011011 */
    { unimplemented, OF_UNIMP},	/* 011100 */
    { "ldstuba", OF_ra_rd },	/* 011101 */
    { unimplemented, OF_UNIMP},	/* 011110 */
    { "swapa", OF_a_rd },	/* 011111 */
    { "ld", OF_a_freg },	/* 100000 (LDF) */
    { "ld", OF_a_fsr },		/* 100001 (LDFSR) */
    { unimplemented, OF_UNIMP},	/* 100010 */
    { "ldd", OF_a_freg },	/* 100011 (LDDF) */
    { "st", OF_freg_a },	/* 100100 (STF) */
    { "st", OF_fsr_a },		/* 100101 (STFSR) */
    { "std", OF_fq_a },		/* 100110 (STFQ) */
    { "std", OF_freg_a },	/* 100111 (STDF) */
    { unimplemented, OF_UNIMP},	/* 101000 */
    { unimplemented, OF_UNIMP},	/* 101001 */
    { unimplemented, OF_UNIMP},	/* 101010 */
    { unimplemented, OF_UNIMP},	/* 101011 */
    { unimplemented, OF_UNIMP},	/* 101100 */
    { unimplemented, OF_UNIMP},	/* 101101 */
    { unimplemented, OF_UNIMP},	/* 101110 */
    { unimplemented, OF_UNIMP},	/* 101111 */
    { "ld", OF_a_creg },	/* 110000 (LDC) */
    { "ld", OF_a_csr },		/* 110001 (LDCSR) */
    { unimplemented, OF_UNIMP},	/* 110010 */
    { "ldd", OF_a_creg },	/* 110011 (LDDC) */
    { "st", OF_creg_a },	/* 110100 (STC) */
    { "st", OF_csr_a },		/* 110101 (STCSR) */
    { "std", OF_cq_a },		/* 110110 (STDCQ) */
    { "std", OF_creg_a },	/* 110111 (STDC) */
    { unimplemented, OF_UNIMP},	/* 111000 */
    { unimplemented, OF_UNIMP},	/* 111001 */
    { unimplemented, OF_UNIMP},	/* 111010 */
    { unimplemented, OF_UNIMP},	/* 111011 */
    { unimplemented, OF_UNIMP},	/* 111100 */
    { unimplemented, OF_UNIMP},	/* 111101 */
    { unimplemented, OF_UNIMP},	/* 111110 */
    { unimplemented, OF_UNIMP}	/* 111111 */
};


/*
 * Bicc suffixes
 */

static char *icc[] = {
    "n",			/* 0000 -- never */
    "e",			/* 0001 -- equal */
    "le",			/* 0010 -- less or equal */
    "l",			/* 0011 -- less */
    "leu",			/* 0100 -- less or equal unsigned */
    "cs",			/* 0101 -- carry set (less than, unsigned) */
    "neg",			/* 0110 -- negative */
    "vs",			/* 0111 -- overflow set */
    "a",			/* 1000 -- always */
    "ne",			/* 1001 -- not equal */
    "g",			/* 1010 -- greater */
    "ge",			/* 1011 -- greater or equal */
    "gu",			/* 1100 -- greater unsigned */
    "cc",			/* 1101 -- carry clear (ge, unsigned) */
    "pos",			/* 1110 -- positive */
    "vc",			/* 1111 -- overflow clear */
};


/*
 * FBfcc suffixes
 */

static char *fcc[] = {
    "n",			/* 0000 -- never */
    "ne",			/* 0001 -- not equal */
    "lg",			/* 0010 -- less or greater */
    "ul",			/* 0011 -- unordered or less */
    "l",			/* 0100 -- less */
    "ug",			/* 0101 -- unordered or greater */
    "g",			/* 0110 -- greater */
    "u",			/* 0111 -- unordered */
    "a",			/* 1000 -- always */
    "e",			/* 1001 -- equal */
    "ue",			/* 1010 -- unordered or equal */
    "ge",			/* 1011 -- greater or equal */
    "uge",			/* 1100 -- unordered or greater or equal */
    "le",			/* 1101 -- less or equal */
    "ule",			/* 1110 -- unordered or less or equal */
    "o",			/* 1111 -- ordered */
};


/*
 * CBccc suffixes
 */

static char *ccc[] = {
    "n",			/* 0000 -- never */
    "123",			/* 0001 -- 1 or 2 or 3 */
    "12",			/* 0010 -- 1 or 2 */
    "13",			/* 0011 -- 1 or 3 */
    "1",			/* 0100 -- 1 */
    "23",			/* 0101 -- 2 or 3 */
    "2",			/* 0110 -- 2 */
    "3",			/* 0111 -- 3 */
    "a",			/* 1000 -- always */
    "0",			/* 1001 -- 0 */
    "03",			/* 1010 -- 0 or 3 */
    "02",			/* 1011 -- 0 or 2 */
    "023",			/* 1100 -- 0 or 2 or 3 */
    "01",			/* 1101 -- 0 or 1 */
    "013",			/* 1110 -- 0 or 1 or 3 */
    "012",			/* 1111 -- 0 or 1 or 2 */
};

static void
print_instruction()
{
    long i;
    i = *ip;
    switch ((i>>30) & 0x3) {
	case 0 : print_format2_instruction(); break;
	case 1 : print_format1_instruction(); break;
	case 2 : print_format3_instruction(op3_10); break;
	case 3 : print_format3_instruction(op3_11); break;
    }
    ip++;
}

static int
offset(val,n)
    int val, n;
{
    if (val & (1<<(n-1)))
	return (val | (-1 << n));
    else /* assuming 32 bit integers... */
	return (val & (((unsigned) 0xffffffff) >> (32-n)));
}

extern	void	wm_unify		PARAMS(( void ));
extern	void	wm_docut		PARAMS(( void ));
extern	void	wm_g_uia		PARAMS(( void ));
extern	void	wm_p_uia		PARAMS(( void ));
extern	void	wm_overflow0		PARAMS(( void ));
extern	void	wm_overflow1		PARAMS(( void ));
extern	void	wm_overflow2		PARAMS(( void ));
extern	void	wm_overflow3		PARAMS(( void ));
extern	void	wm_resolve_ref		PARAMS(( void ));
extern	void	wm_try0			PARAMS(( void ));
extern	void	wm_try1			PARAMS(( void ));
extern	void	wm_try2			PARAMS(( void ));
extern	void	wm_try3			PARAMS(( void ));
extern	void	wm_retry_u0		PARAMS(( void ));
extern	void	wm_retry_u1		PARAMS(( void ));
extern	void	wm_retry_u2		PARAMS(( void ));
extern	void	wm_retry_u3		PARAMS(( void ));
extern	void	wm_retry0		PARAMS(( void ));
extern	void	wm_retry1		PARAMS(( void ));
extern	void	wm_retry2		PARAMS(( void ));
extern	void	wm_retry3		PARAMS(( void ));
extern	void	wm_trust_u0		PARAMS(( void ));
extern	void	wm_trust_u1		PARAMS(( void ));
extern	void	wm_trust_u2		PARAMS(( void ));
extern	void	wm_trust_u3		PARAMS(( void ));
extern	void	wm_trust0		PARAMS(( void ));
extern	void	wm_trust1		PARAMS(( void ));
extern	void	wm_trust2		PARAMS(( void ));
extern	void	wm_trust3		PARAMS(( void ));
extern	void	wm_exec_builtin0	PARAMS(( void ));
extern	void	wm_exec_builtin1	PARAMS(( void ));
extern	void	wm_exec_builtin2	PARAMS(( void ));
extern	void	wm_exec_builtin3	PARAMS(( void ));
extern	void	wm_g_sym		PARAMS(( void ));
extern	void	wm_u_sym		PARAMS(( void ));
extern	void	mth_pushdbl0		PARAMS(( void ));
extern	void	wm_p_unsafe		PARAMS(( void ));
extern	void	wm_u_lval		PARAMS(( void ));
extern	void	wm_u_int		PARAMS(( void ));


#define LASTUIAINSTR 1

static struct nstruct {
   void (*addr) PARAMS(( void ));
   char *name;
} snames[] =
{
	{ wm_g_uia, "wm_g_uia" },
	{ wm_p_uia, "wm_p_uia" },	/* This is LASTUIAINSTR */
	{ wm_unify, "wm_unify" },
	{ wm_docut, "wm_docut" },
	{ wm_resolve_ref, "wm_resolve_ref" },
	{ wm_try0,	"wm_try0" },
	{ wm_try1,	"wm_try1" },
	{ wm_try2,	"wm_try2" },
	{ wm_try3,	"wm_try3" },
	{ wm_retry_u0,	"wm_retry_u0" },
	{ wm_retry_u1,	"wm_retry_u1" },
	{ wm_retry_u2,	"wm_retry_u2" },
	{ wm_retry_u3,	"wm_retry_u3" },
	{ wm_retry0,	"wm_retry0" },
	{ wm_retry1,	"wm_retry1" },
	{ wm_retry2,	"wm_retry2" },
	{ wm_retry3,	"wm_retry3" },
	{ wm_trust_u0,	"wm_trust_u0" },
	{ wm_trust_u1,	"wm_trust_u1" },
	{ wm_trust_u2,	"wm_trust_u2" },
	{ wm_trust_u3,	"wm_trust_u3" },
	{ wm_trust0,	"wm_trust0" },
	{ wm_trust1,	"wm_trust1" },
	{ wm_trust2,	"wm_trust2" },
	{ wm_trust3,	"wm_trust3" },
	{ wm_exec_builtin0, "wm_exec_builtin0" },
	{ wm_exec_builtin1, "wm_exec_builtin1" },
	{ wm_exec_builtin2, "wm_exec_builtin2" },
	{ wm_exec_builtin3, "wm_exec_builtin3" },
	{ wm_overflow0,	"wm_overflow0" },
	{ wm_overflow1,	"wm_overflow1" },
	{ wm_overflow2,	"wm_overflow2" },
	{ wm_overflow3,	"wm_overflow3" },
	{ wm_g_sym,	"wm_g_sym" },
	{ wm_u_sym,	"wm_u_sym" },
	{ mth_pushdbl0,	"mth_pushdbl0" },
	{ wm_p_unsafe,	"wm_p_unsafe"},
	{ wm_u_lval,	"wm_u_lval"},
	{ wm_u_int,	"wm_u_int"}

};


static char *
symbolic_name(addr)
   long *addr;
{
   register int i;
   static char buf[100];
   char *res;
   for (i=0; i < ((sizeof snames) / sizeof (struct nstruct)); i++)
      if ( ((long *) (((long ) snames[i].addr) & ~3)) == addr ) {
	 if (addr == (long *) mth_pushdbl0) {
	    sprintf(buf, "%s\n\tDOUBLE(%g)", snames[i].name, * (double *) (ip+2));
	    ip += 3;
	    return buf;
	 }
	 else if (i > LASTUIAINSTR)
	    return snames[i].name;
	 else {
	    sprintf(buf, "%s\n\tUIANAME(%s)", snames[i].name, (char *)(ip+3));
	    ip = ip + (*(ip+2) >> 4)+1;
	    return buf;
	 }
      }
   
   if ( (res=w_getnamestring(addr, buf)) )
      return res;
   
   sprintf(buf, "%#lx", (long) addr);
   return buf;
}


static void
print_format1_instruction()
{
    PI_printf("call\t%s\n",(int) symbolic_name(ip+offset(*ip,30)));
}

#define MASK1 0x1
#define MASK2 0x3
#define MASK3 0x7
#define MASK4 0xf
#define MASK5 0x1f
#define MASK6 0x3f
#define MASK8 0xff
#define MASK13 0x1fff
#define MASK22 0x3fffff

#define REG_D(i) (((i) >> 25) & MASK5)
#define OP2(i) (((i) >> 22) & MASK3)
#define COND(i) (((i) >>25) & MASK4)
#define ANNUL(i) (((i) >> 29) & MASK1)

#define OP3(i) (((i) >> 19) & MASK6)
#define REG_S1(i) (((i) >> 14) & MASK5)
#define REG_S2(i) ((i) & MASK5)
#define ASI(i) (((i) >> 5) & MASK8)
#define SIMM13(i) offset(i,13)
#define I(i) (((i) >> 13) & MASK1)


static void
print_format2_instruction()
{
    long i;
    i = *ip;
    switch(OP2(i)) {
	case 0 :		/* UNIMP */
	    PI_printf("unimp\t0x%x\n", i & MASK22);
	    break;
	case 2 :		/* Bicc */
	    print_conditional_instr("b",icc,i);
	    break;
	case 4 :		/* SETHI */
	    PI_printf("sethi\t0x%x, %s\n",(i & MASK22), regs[REG_D(i)]);
	    break;
	case 6 :		/* FBfcc */
	    print_conditional_instr("fb",fcc,i);
	    break;
	case 7 :
	    print_conditional_instr("cb",ccc,i);
	    break;
	default :
	    print_unimplemented(i);
	    break;
    }
}

static void
print_conditional_instr(prefix,ctab,instr)
    char *prefix;
    char **ctab;
    long instr;
{
    PI_printf(	"%s%s%s\t0x%x\n",
		prefix,
		ctab[COND(instr)],
		(ANNUL(instr) ? ",a" : ""),
		ip + offset(instr,22)	);
}

static void
print_unimplemented(instr)
    long instr;
{
    PI_printf("%s\t\t(0x%x)\n",unimplemented,instr);
}

static char *
getaddress(instr)
    long instr;
{
    static char buf[64];
    int rs1;

    rs1 = REG_S1(instr);
    if (I(instr)) {
	int simm13 = SIMM13(instr);
	if (!simm13) {
	    sprintf(buf,"%s",regs[rs1]);
	}
	else if (!rs1) {
	    sprintf(buf,"0x%x",simm13);
	}
	else if (simm13 < 0) {
	    sprintf(buf,"%s-%d",regs[rs1],-simm13);
	}
	else {
	    sprintf(buf,"%s+%d",regs[rs1],simm13);
	}
    }
    else {
	int rs2 = REG_S2(instr);
	if (!rs2) {
	    sprintf(buf,"%s",regs[rs1]);
	}
	else if (!rs1) {
	    sprintf(buf,"%s",regs[rs2]);
	}
	else {
	    sprintf(buf,"%s+%s",regs[rs1],regs[rs2]);
	}
    }
    return buf;
}

static char *
reg_or_imm(instr)
    long instr;
{
    static char buf[32];
    if (I(instr)) {
	sprintf(buf,"0x%x",SIMM13(instr));
    }
    else {
	sprintf(buf,"%s",regs[REG_S2(instr)]);
    }

    return buf;
}

static void
print_format3_instruction(itab)
    struct iinfo *itab;
{
    long instr;
    int idx;

    instr = *ip;
    idx = (instr >> 19) & 0x3f;

    switch (itab[idx].format) {
	case OF_UNIMP	:		/* opcode is not implemented */
	    print_unimplemented(instr);
	    break;

	case OF_a_rd :			/* [address], reg */
	    PI_printf("%s\t[%s], %s\n",
		    itab[idx].name,
		    getaddress(instr),
		    regs[REG_D(instr)]);
	    break;

	case OF_ra_rd :			/* [regaddr] asi, reg */
	    PI_printf("%s\t[%s] %d, %s\n",
		    itab[idx].name,
		    getaddress(instr),
		    ASI(instr),
		    regs[REG_D(instr)]);
	    break;
	
	case OF_a_freg :		/* [address], freg */
	    PI_printf("%s\t[%s], %%f%d\n",
		    itab[idx].name,
		    getaddress(instr),
		    REG_D(instr));
	    break;
	
	case OF_a_fsr :			/* [address], %fsr */
	    PI_printf("%s\t[%s], %%fsr\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;
	
	case OF_a_creg :		/* [address], creg */
	    PI_printf("%s\t[%s], %%c%d\n",
		    itab[idx].name,
		    getaddress(instr),
		    REG_D(instr));
	    break;
	
	case OF_a_csr :			/* [address], csr */
	    PI_printf("%s\t[%s], %%csr\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;
	
	case OF_rd_a :			/* reg, [address] */
	    PI_printf("%s\t%s, [%s]\n",
		    itab[idx].name,
		    regs[REG_D(instr)],
		    getaddress(instr));
	    break;
	case OF_rd_ra :			/* reg, [regaddr] asi */
	    PI_printf("%s\t%s, [%s] %d\n",
		    itab[idx].name,
		    regs[REG_D(instr)],
		    getaddress(instr),
		    ASI(instr));
	    break;
	
	case OF_freg_a :		/* freg, [address] */
	    PI_printf("%s\t%%f%d, [%s]\n",
		    itab[idx].name,
		    REG_D(instr),
		    getaddress(instr));
	    break;
	
	case OF_fsr_a :			/* %fsr, [address] */
	    PI_printf("%s\t%%fsr, [%s]\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;

	case OF_fq_a :			/* %fq, [address] */
	    PI_printf("%s\t%%fq, [%s]\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;
	
	case OF_creg_a :		/* creg, [address] */
	    PI_printf("%s\t%%c%d, [%s]\n",
		    itab[idx].name,
		    REG_D(instr),
		    getaddress(instr));
	    break;

	case OF_csr_a :			/* %csr, [address] */
	    PI_printf("%s\t%%csr, [%s]\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;

	case OF_cq_a :			/* %cq, [address] */
	    PI_printf("%s\t%%cq, [%s]\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;
	
	case OF_r_ri_rd :		/* reg, reg_or_imm, reg */
	    PI_printf("%s\t%s, %s, %s\n",
		    itab[idx].name,
		    regs[REG_S1(instr)],
		    reg_or_imm(instr),
		    regs[REG_D(instr)]);
	    break;
	
	case OF_a :			/* address */
	    PI_printf("%s\t%s\n",
		    itab[idx].name,
		    getaddress(instr));
	    break;
	
	case OF_y_rd :			/* %y, reg */
	    PI_printf("%s\t%%y, %s\n",
		    itab[idx].name,
		    regs[REG_D(instr)]);
	    break;
	
	case OF_psr_rd :		/* %psr, reg */
	    PI_printf("%s\t%%psr, %s\n",
		    itab[idx].name,
		    regs[REG_D(instr)]);
	    break;
	
	case OF_wim_rd :		/* %wim, reg */
	    PI_printf("%s\t%%wim, %s\n",
		    itab[idx].name,
		    regs[REG_D(instr)]);
	    break;
	
	case OF_tbr_rd :		/* %tbr, reg */
	    PI_printf("%s\t%%tbr, %s\n",
		    itab[idx].name,
		    regs[REG_D(instr)]);
	    break;
	
	case OF_r_ri_y :		/* reg, reg_or_imm, %y */
	    PI_printf("%s\t%s, %s, %%y\n",
		    itab[idx].name,
		    regs[REG_S1(instr)],
		    reg_or_imm(instr));
	    break;

	case OF_r_ri_psr :		/* reg, reg_or_imm, %psr */
	    PI_printf("%s\t%s, %s, %%psr\n",
		    itab[idx].name,
		    regs[REG_S1(instr)],
		    reg_or_imm(instr));
	    break;

	case OF_r_ri_wim :		/* reg, reg_or_imm, %wim */
	    PI_printf("%s\t%s, %s, %%wim\n",
		    itab[idx].name,
		    regs[REG_S1(instr)],
		    reg_or_imm(instr));
	    break;

	case OF_r_ri_tbr :		/* reg, reg_or_imm, %tbr */
	    PI_printf("%s\t%s, %s, %%tbr\n",
		    itab[idx].name,
		    regs[REG_S1(instr)],
		    reg_or_imm(instr));
	    break;

	case OF_ticc :			/* address (for ticc instruction) */
	    PI_printf("t%s\t%s\n",
		    icc[COND(instr)],
		    getaddress(instr));
	    break;

	case OF_fpop1 :
	case OF_fpop2 :
	case OF_cpop1 :
	case OF_cpop2 :
	    PI_printf("fpop or cpop\t(0x%x)\n",instr);
	    break;
	
	default :
	    PI_printf("Unrecognized format: %d\n",itab[idx].format);
	    break;

    }
}
