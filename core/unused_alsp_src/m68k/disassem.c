/*
 * disassem.c           -- 68020 disassembler
 *      Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/17/87
 * 10/26/94,	C. Houpt -- Redefined the Instruction Pointer as a union so
 *				that it can be used as both a short and long pointer.  This
 *				avoid the need to casting l-values, which is not ANSI.
 */

#include "defs.h"
#include "wintcode.h"

static	void	print_instruction	PARAMS(( void ));
static	void	decode_immed		PARAMS(( int ));
static	void	decode_move		PARAMS(( int ));
static	void	decode_misc		PARAMS(( int ));
static	void	decode_quick		PARAMS(( int ));
static	void	decode_branch		PARAMS(( int ));
static	void	decode_moveq		PARAMS(( int ));
static	void	decode_or		PARAMS(( int ));
static	void	decode_sub		PARAMS(( int ));
static	void	decode_reserved		PARAMS(( int ));
static	void	decode_cmp		PARAMS(( int ));
static	void	decode_and		PARAMS(( int ));
static	void	decode_add		PARAMS(( int ));
static	void	decode_shift		PARAMS(( int ));
static	void	decode_coproc		PARAMS(( int ));
static	char *	symbolic_name		PARAMS(( long ));
static	char *	EAstr			PARAMS(( int, int, int ));

#ifdef notdef
#define PI_printf printf
main()
{
    list_asm(main, 100);
}
#endif

/* static Code *ip; */

/* Define the Instruction Pointer (uip) as a union so that it can be used
   as both a short and long pointer.
*/
union {
	Code *c;
	long *l;
}	uip;

/* Define ip as a code pointer to handle the common case of fetching Code. */
#define ip	uip.c

void
list_asm(addr, n)
    Code *addr;		/* Start address */
    int   n;
{
    Code *stopaddr = addr + n;

    ip = addr;
    while (ip < stopaddr) {
	PI_printf("%x:\t", (int) ip);
	print_instruction();
    }
}


static void
print_instruction()
{
    int   opcode;

    opcode = (*ip++) & 0xffff;	/* get out opcode */

    switch (opcode >> 12) {
	case 0:
	    decode_immed(opcode);
	    break;
	case 1:
	case 2:
	case 3:
	    decode_move(opcode);
	    break;
	case 4:
	    decode_misc(opcode);
	    break;
	case 5:
	    decode_quick(opcode);
	    break;
	case 6:
	    decode_branch(opcode);
	    break;
	case 7:
	    decode_moveq(opcode);
	    break;
	case 8:
	    decode_or(opcode);
	    break;
	case 9:
	    decode_sub(opcode);
	    break;
	case 10:
	    decode_reserved(opcode);
	    break;
	case 11:
	    decode_cmp(opcode);
	    break;
	case 12:
	    decode_and(opcode);
	    break;
	case 13:
	    decode_add(opcode);
	    break;
	case 14:
	    decode_shift(opcode);
	    break;
	case 15:
	    decode_coproc(opcode);
	    break;
    }
}


#define BUFSIZE 100
char *sizes[] =
{"b", "w", "l"};
char *bitops[] =
{"tst", "chg", "clr", "set"};
char *regnames[] =
{"D0", "D1", "D2", "D3", "D4", "D5", "D6", "D7",
 "A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"};
char *immednames[] =
{"or", "and", "sub", "add", "", "eor", "cmp"};

void
decode_immed(opcode)
    int   opcode;
{
    int   mode, reg, size, what;
    int   data = 0;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    what = (opcode >> 9) & 7;

    if ((opcode & 0xff00) == 0x0800) {	/* Static Bit */
	data = (*ip++) & 0xff;
	PI_printf("b%s\t#%d, %s\n", bitops[size], data, EAstr(mode, reg, 0));
    }
    else if (mode == 1) {	/* movep instruction */
	data = *ip++;		/* get displacement */
	switch ((opcode >> 6) & 7) {
	    case 4:
		PI_printf("movep.w\t(%d,A%d), D%d\n", data, reg, what);
		break;
	    case 5:
		PI_printf("movep.l\t(%d,A%d), D%d\n", data, reg, what);
		break;
	    case 6:
		PI_printf("movep.w\tD%d, (%d,A%d)\n", what, data, reg);
		break;
	    case 7:
		PI_printf("movep.l\tD%d, (%d,A%d)\n", what, data, reg);
		break;
	}
    }
    else if (opcode & 0x100) {	/* Dynamic Bit */
	PI_printf("b%s\tD%d, %s\n", bitops[size], what, EAstr(mode, reg, 0));
	return;
    }
    else if (size == 3) {	/* cmp2, chk2, rtm, callm, cas, or cas2 */
	size = (opcode >> 9) & 3;
	if (opcode & 0x0800) {	/* cas, cas2 */
	    data = *ip++;
	    if (mode == 7 && reg == 4) {	/* cas2 */
		int   data2;

		data2 = *ip++;
		PI_printf("cas2\tD%d:D%d,D%d:D%d,(%s%d):(%s%d)\n",
			  data & 7,
			  data2 & 7,
			  (data >> 6) & 7,
			  (data2 >> 6) & 7,
			  (data & 0x8000) ? "A" : "D",
			  (data >> 12) & 7,
			  (data2 & 0x8000) ? "A" : "D",
			  (data2 >> 12) & 7);
	    }
	    else
		PI_printf("cas\tD%d, D%d, %s\n", data & 7, (data >> 6) & 7, EAstr(mode, reg, 0));
	}
	else {
	    if (size != 3) {
		data = *ip++;
		PI_printf("%s.%s\t%s, %s\n", (data & 0x800) ? "chk2" : "cmp2",
			  sizes[size],
			  regnames[(data >> 12)],
			  EAstr(mode, reg, 0));
	    }
	    else {		/* callm and rtm */
		if ((mode & ~1) == 0)
		    PI_printf("rtm\t%s%d\n", mode ? "A" : "D", reg);
		else {
		    data = (*ip++) & 0xff;
		    PI_printf("callm\t#%d, %s\n", data, EAstr(mode, reg, 0));
		}
	    }
	}
    }
    else if (what == 7) {
	data = *ip++;
	if (data & 0x0800)
	    PI_printf("moves.%s\t%s, %s\n", sizes[size],
		      regnames[(data >> 12)], EAstr(mode, reg, size));
	else
	    PI_printf("moves.%s\t%s, %s\n", sizes[size], EAstr(mode, reg, size),
		      regnames[(data >> 12)]);
    }
    else {			/* or, and, sub, add, eor, cmp */
	switch (size) {
	    case 0:
		data = (*ip++) & 0xff;
		break;
	    case 1:
		data = *ip++;
		break;
	    case 2:
		data = *uip.l++;
		break;
	}
	if (mode == 7 && reg == 4)
	    if (size == 0)
		PI_printf("%s\t#%d, CCR\n", immednames[what], data);
	    else
		PI_printf("%s\t#%d, SR\n", immednames[what], data);
	else
	    PI_printf("%s.%s\t#%d, %s\n", immednames[what],
		      sizes[size],
		      data,
		      EAstr(mode, reg, 0));
    }
}

static void
decode_move(opcode)
    int   opcode;
{
    int   size;
    int   dreg, sreg;
    int   dmode, smode;
    char  srcEA[BUFSIZE];

    size = ((opcode >> 12) & 3);
    if (size == 1)
	size = 0;
    else if (size == 3)
	size = 1;
    else
	size = 2;

    sreg = opcode & 7;
    smode = ((opcode >> 3) & 7);
    dmode = ((opcode >> 6) & 7);
    dreg = ((opcode >> 9) & 7);

    strcpy(srcEA, EAstr(smode, sreg, size));
    PI_printf("move.%s\t%s, %s\n", sizes[size], srcEA, EAstr(dmode, dreg, size));
}

static char *misc716X[] =
{"reset", "nop", "stop", "rte", "rtd", "rts", "trapv", "rtr"};

static void
decode_misc(opcode)
    int   opcode;
{
    int   data;
    int   size = (opcode >> 6) & 3;
    int   mode = (opcode >> 3) & 7;
    int   reg = opcode & 7;

    switch ((opcode >> 6) & 0x3f) {
	case 0:
	case 1:
	case 2:
	    PI_printf("negx.%s\t%s\n", sizes[size], EAstr(mode, reg, size));
	    break;
	case 3:
	    PI_printf("move\tSR, %s\n", EAstr(mode, reg, size));
	    break;
	case 004:
	case 014:
	case 024:
	case 034:
	case 044:
	case 054:
	case 064:
	case 074:
	    PI_printf("chk.l\t%s, D%d\n", EAstr(mode, reg, 0), (opcode >> 9) & 7);
	    break;
	case 006:
	case 016:
	case 026:
	case 036:
	case 046:
	case 056:
	case 066:
	case 076:
	    PI_printf("chk.w\t%s, D%d\n", EAstr(mode, reg, 0), (opcode >> 9) & 7);
	    break;
	case 007:
	case 017:
	case 027:
	case 037:
	case 047:
	case 057:
	case 067:
	case 077:
	    if (((opcode >> 6) & 0x3f) && mode == 0)
		PI_printf("extb.l\tD%d\n", reg);
	    else
		PI_printf("lea\t%s, A%d\n", EAstr(mode, reg, 0), (opcode >> 9) & 7);
	    break;
	case 010:
	case 011:
	case 012:
	    PI_printf("clr.%s\t%s\n", sizes[size], EAstr(mode, reg, 0));
	    break;
	case 013:
	    PI_printf("move\tCCR, %s\n", EAstr(mode, reg, 0));
	    break;
	case 020:
	case 021:
	case 022:
	    PI_printf("neg.%s\t%s\n", sizes[size], EAstr(mode, reg, 0));
	    break;
	case 023:
	    PI_printf("move\t%s, CCR\n", EAstr(mode, reg, 0));
	    break;
	case 030:
	case 031:
	case 032:
	    PI_printf("not.%s\t%s\n", sizes[size], EAstr(mode, reg, 0));
	    break;
	case 033:
	    PI_printf("move\t%s, SR\n", EAstr(mode, reg, 0));
	    break;
	case 040:
	    if (mode == 1)
		PI_printf("link\tA%d, #%d.l\n", reg, *uip.l++);
	    else
		PI_printf("nbcd\t%s\n", EAstr(mode, reg, 0));
	    break;
	case 041:
	    if (mode == 0)
		PI_printf("swap\tD%d\n", reg);
	    else if (mode == 1)
		PI_printf("bkpt\t#%d\n", reg);
	    else
		PI_printf("pea\t%s\n", EAstr(mode, reg, 0));
	    break;
	case 042:
	case 043:
	    if (mode == 0)
		PI_printf("ext.%s\tD%d\n", sizes[size - 1], reg);
	    else {
		data = *ip++;
		PI_printf("movem.%s\t#%d, %s\n", sizes[size - 1], data, EAstr(mode, reg, 0));
	    }
	    break;
	case 050:
	case 051:
	case 052:
	    PI_printf("tst.%s\t%s\n", sizes[size], EAstr(mode, reg, 0));
	    break;
	case 053:
	    if (mode == 7 && reg == 4)
		PI_printf("illegal\n");
	    else
		PI_printf("tas\t%s\n", EAstr(mode, reg, 0));
	    break;
	case 060:
	    data = *ip++;
	    if (data & 0x400)
		PI_printf("mul%s.l\t%s, D%d:D%d\n", (data & 0x800) ? "s" : "u",
			  EAstr(mode, reg, 0),
			  data & 7,
			  data >> 12);
	    else
		PI_printf("mul%s.l\t%s, D%d\n", (data & 0x800) ? "s" : "u",
			  EAstr(mode, reg, 0),
			  data >> 12);
	    break;
	case 061:
	    data = *ip++;
	    PI_printf("div%s%s.l\t%s, D%d:D%d\n",
		      (data & 0x400) ? "" : "l",
		      (data & 0x800) ? "s" : "u",
		      EAstr(mode, reg, 0),
		      data & 7,
		      data >> 12);
	    break;
	case 062:
	case 063:
	    data = *ip++;
	    PI_printf("movem\t%s, #%d\n", EAstr(mode, reg, 0), data);
	    break;
	case 071:
	    switch (mode) {
		case 0:
		case 1:
		    PI_printf("trap\t#%d\n", (mode << 3) + reg);
		    break;
		case 2:
		    PI_printf("link\tA%d, #%d\n", reg, *ip++);
		    break;
		case 3:
		    PI_printf("unlk\tA%d\n", reg);
		    break;
		case 4:
		    PI_printf("move\tA%d, USP\n", reg);
		    break;
		case 5:
		    PI_printf("move\tUSP, A%d\n", reg);
		    break;
		case 6:
		    PI_printf("%s\n", misc716X[reg]);
		    break;
		case 7:
		    data = *ip++;
		    PI_printf("movec\t#%d (decode it yourself)\n", data);
		    break;
	    }
	    break;
	case 072:
	    PI_printf("jsr\t%s\n", EAstr(mode, reg, 0));
	    break;
	case 073:
	    PI_printf("jmp\t%s\n", EAstr(mode, reg, 0));
	    break;

    }
}

static char *condition_codes[] =
{"t", "f", "hi", "ls", "hs", "lo", "ne", "eq",
 "vc", "vs", "pl", "mi", "ge", "lt", "gt", "le"};

static void
decode_quick(opcode)
    int   opcode;
{
    int   reg;
    int   mode;
    int   size;
    int   condition;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    condition = (opcode >> 8) & 0x0f;

    if (size != 3) {
	PI_printf("%s.%s\t#%d, %s\n", (opcode & 0x100) ? "subq" : "addq",
		  sizes[size],
		  (condition >> 1) ? (condition >> 1) : 8,
		  EAstr(mode, reg, 0));
    }
    else if (mode == 1) {
	long  addr = (long) ip;
	int   disp;

	disp = *ip++;
	if (disp & 0x8000)
	    disp |= 0xffff0000;
	addr += disp;
	PI_printf("db%s\t(%x)\n", condition_codes[condition], addr);
    }
    else if (mode == 7) {
	if (reg == 4)
	    PI_printf("trap%s\n", condition_codes[condition]);
	else if (reg == 2)
	    PI_printf("trap%s.w\t#%d\n", condition_codes[condition], *ip++);
	else
	    PI_printf("trap%s.l\t#%d\n", condition_codes[condition], *uip.l++);
    }
    else
	PI_printf("s%s\t%s\n", condition_codes[condition], EAstr(mode, reg, 0));
}


static void
decode_branch(opcode)
    int   opcode;
{
    long  addr = (long) ip;
    int   disp;
    int   condition;
    char  size;

    disp = opcode & 0xff;
    condition = (opcode >> 8) & 0xf;
    if (disp == 0) {
	disp = *ip++;
	if (disp & 0x8000)
	    disp |= 0xffff0000;
	size = 'w';
    }
    else if (disp == 0xff) {
	disp = *uip.l++;
	size = 'l';
    }
    else {
	if (disp & 0x80)
	    disp |= 0xffffff00;
	size = 's';
    }
    addr += disp;
    if (condition == 0)
	PI_printf("bra.%c\t%s\n", size, symbolic_name(addr));
    else if (condition == 1)
	PI_printf("bsr.%c\t%s\n", size, symbolic_name(addr));
    else
	PI_printf("b%s.%c\t%s\n", condition_codes[condition], size,
		  symbolic_name(addr));
}

static void
decode_moveq(opcode)
    int   opcode;
{
    int   data;

    data = opcode & 0xff;
    if (data & 0x80)
	data |= 0xffffff00;
    PI_printf("moveq\t#%d, D%d\n", data, (opcode >> 9) & 7);
}

static void
decode_or(opcode)
    int   opcode;
{
    int   reg;
    int   mode;
    int   dreg;
    int   size;
    int   type;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    dreg = (opcode >> 9) & 7;
    type = (opcode >> 8) & 1;

    if (size == 3)		/* divu, divs */
	PI_printf("div%s.w\t%s, D%d\n", type ? "s" : "u", EAstr(mode, reg, 1), dreg);
    else if (type && (mode == 0 || mode == 1)) {
	switch (size) {
	    case 0:
		if (mode == 0)
		    PI_printf("sbcd\tD%d, D%d\n", reg, dreg);
		else
		    PI_printf("sbcd\t-(A%d), -(A%d)\n", reg, dreg);
		break;
	    case 1:
		if (mode == 0)
		    PI_printf("pack\tD%d, D%d, #%d\n", reg, dreg, *ip++);
		else
		    PI_printf("pack\t-(A%d), -(A%d), #%d\n", reg, dreg, *ip++);
		break;
	    case 2:
		if (mode == 0)
		    PI_printf("unpk\tD%d, D%d, #%d\n", reg, dreg, *ip++);
		else
		    PI_printf("unpk\t-(A%d), -(A%d), #%d\n", reg, dreg, *ip++);
		break;
	}
    }
    else if (type)
	PI_printf("or.%s\tD%d, %s\n", sizes[size], dreg, EAstr(mode, reg, size));
    else
	PI_printf("or.%s\t%s, D%d\n", sizes[size], EAstr(mode, reg, size), dreg);
}


static void
decode_sub(opcode)
    int   opcode;
{
    int   reg;
    int   mode;
    int   dreg;
    int   size;
    int   type;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    dreg = (opcode >> 9) & 7;
    type = (opcode >> 8) & 1;

    if (type && mode == 0 && size != 3)
	PI_printf("subx.%s\tD%d, D%d\n", sizes[size], reg, dreg);
    else if (type && mode == 1 && size != 3)
	PI_printf("subx.%s\t-(A%d), -(A%d)\n", sizes[size], reg, dreg);
    else if (size == 3)
	PI_printf("sub.%s\t%s, A%d\n", sizes[type + 1], EAstr(mode, reg, type + 1), dreg);
    else if (type)
	PI_printf("sub.%s\tD%d, %s\n", sizes[size], dreg, EAstr(mode, reg, size));
    else
	PI_printf("sub.%s\t%s, D%d\n", sizes[size], EAstr(mode, reg, size), dreg);
}


static void
decode_reserved(opcode)
    int   opcode;
{
    PI_printf("Reserved Instruction: %x\n", opcode);
}

static void
decode_cmp(opcode)
    int   opcode;
{
    int   reg;
    int   mode;
    int   dreg;
    int   size;
    int   type;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    dreg = (opcode >> 9) & 7;
    type = (opcode >> 8) & 1;

    if (size == 3)
	PI_printf("cmp.%s\t%s, A%d\n", sizes[type + 1], EAstr(mode, reg, type + 1), dreg);
    else if (type) {
	if (mode == 1)
	    PI_printf("cmpm\t(A%d)+, (A%d)+\n", reg, dreg);
	else
	    PI_printf("eor.%s\tD%d, %s\n", sizes[size], dreg, EAstr(mode, reg, size));
    }
    else
	PI_printf("cmp.%s\t%s, D%d\n", sizes[size], EAstr(mode, reg, size), dreg);
}

static void
decode_and(opcode)
    int   opcode;
{
    int   reg;
    int   mode;
    int   dreg;
    int   size;
    int   type;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    dreg = (opcode >> 9) & 7;
    type = (opcode >> 8) & 1;

    if (size == 3)
	PI_printf("mul%s.w\t%s, D%d\n", type ? "s" : "u", EAstr(mode, reg, 1), dreg);
    else if (type && (mode == 0 || mode == 1)) {
	if (size == 0) {	/* abcd */
	    if (mode)
		PI_printf("abcd\t-(A%d), -(A%d)\n", reg, dreg);
	    else
		PI_printf("abcd\tD%d, D%d\n", reg, dreg);
	}
	else if (size == 1)
	    PI_printf("exg\t%s%d, %s%d\n", mode ? "A" : "D", dreg, mode ? "A" : "D", reg);
	else
	    PI_printf("exg\tD%d, A%d\n", dreg, reg);
    }
    else if (type)
	PI_printf("and.%s\tD%d, %s\n", sizes[size], dreg, EAstr(mode, reg, size));
    else
	PI_printf("and.%s\t%s, D%d\n", sizes[size], EAstr(mode, reg, size), dreg);
}

static void
decode_add(opcode)
    int   opcode;
{
    int   reg;
    int   mode;
    int   dreg;
    int   size;
    int   type;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    dreg = (opcode >> 9) & 7;
    type = (opcode >> 8) & 1;

    if (type && (mode == 0 || mode == 1)) {
	if (mode)
	    PI_printf("addx.%s\t-(A%d), -(A%d)\n", sizes[size], reg, dreg);
	else
	    PI_printf("addx.%s\tD%d, D%d\n", sizes[size], reg, dreg);
    }
    else if (size == 3)
	PI_printf("add.%s\t%s, A%d\n", sizes[type + 1], EAstr(mode, reg, type + 1), dreg);
    else if (type)
	PI_printf("add.%s\tD%d, %s\n", sizes[size], dreg, EAstr(mode, reg, size));
    else
	PI_printf("add.%s\t%s, D%d\n", sizes[size], EAstr(mode, reg, size), dreg);
}

static char *shiftinstrs[] =
{"as", "ls", "ro", "rox"};
char *bfinstrs[] =
{"bftst", "bfextu", "bfchg", "bfexts", "bfclr",
 "bfffo", "bfset", "bfins"};

static void
decode_shift(opcode)
    int   opcode;
{
    int   size;
    int   reg;
    int   mode;
    int   count;

    reg = opcode & 7;
    mode = (opcode >> 3) & 7;
    size = (opcode >> 6) & 3;
    count = (opcode >> 9) & 7;

    if (size == 3) {
	if (opcode & 0x800) {
	    int   data;

	    data = *ip++;
	    PI_printf("%s\t{%s%d:%s%d},%s\n",
		      bfinstrs[((opcode >> 8) & 7)],
		      (0x800 & data) ? "D" : "#",
		      (data >> 6) & 0x1f,
		      (0x20 & data) ? "D" : "#",
		      data & 0x1f,
		      EAstr(mode, reg, 0));
	}
	else {
	    PI_printf("%s%s\t%s\n", shiftinstrs[((opcode >> 9) & 3)],
		      (0x100 & opcode) ? "l" : "r",
		      EAstr(mode, reg, 0));
	}
    }
    else
	PI_printf("%s%s.%s\t%s%d, D%d\n", shiftinstrs[(mode & 3)],
		  (0x100 & opcode) ? "l" : "r",
		  sizes[size],
		  (mode & 4) ? "D" : "#",
		  (opcode >> 9) & 7,
		  reg);
}

void
decode_coproc(opcode)
    int   opcode;
{
}


extern	int	wm_g_uia		PARAMS(( void ));
extern	int	wm_p_uia		PARAMS(( void ));
extern	int	wm_unify		PARAMS(( void ));
extern	int	wm_docut		PARAMS(( void ));
extern	int	wm_overflow		PARAMS(( void ));
extern	int	wm_resolve_ref		PARAMS(( void ));
extern	int	wm_execute_builtin	PARAMS(( void ));
extern	int	wm_try_me		PARAMS(( void ));
extern	int	wm_retry_me		PARAMS(( void ));
extern	int	wm_trust_me		PARAMS(( void ));
extern	int	wm_fail			PARAMS(( void ));


#define LASTUIAINSTR 1

static struct nstruct {
    int (*addr) PARAMS(( void ));
    char *name;
} snames[] =

{
    {
	wm_g_uia, "wm_g_uia"
    },
    {
	wm_p_uia, "wm_p_uia"
    },				/* This is LASTUIAINSTR */
    {
	wm_unify, "wm_unify"
    },
    {
	wm_docut, "wm_docut"
    },
    {
	wm_overflow, "wm_overflow"
    },
    {
	wm_resolve_ref, "wm_resolve_ref"
    },
    {
	wm_execute_builtin, "wm_execute_builtin"
    },
    {
	wm_try_me, "wm_try_me"
    },
    {
	wm_retry_me, "wm_retry_me"
    },
    {
	wm_trust_me, "wm_trust_me"
    },
    {
	wm_fail, "wm_fail"
    }

};

static char *
symbolic_name(addr)
    long addr;
{
    register int i;
    static char buf[BUFSIZE];
    char *res;

    for (i = 0; i < ((sizeof snames) / sizeof (struct nstruct)); i++)
	if (((long) snames[i].addr) == addr) {
	    if (i > LASTUIAINSTR)
		return snames[i].name;
	    else {
		sprintf(buf, "%s\n\tUIANAME(%s)", snames[i].name, 
		        (char *)(ip + 2));
		ip = ip + 2 * (*(ip + 1) & 0xffff);
		return buf;
	    }
	}

    if ((res = w_getnamestring((Code *)addr, buf)))
	return res;

    sprintf(buf, "(0x%lx)", addr);
    return buf;
}



static char *
EAstr(mode, reg, size)
    int   mode;
    int   reg;
    int   size;			/* used for immediate data */
{
    static char buf[BUFSIZE];
    int   ext;			/* extension */
    int   bd = 0;		/* base displacement */
    int   isuppress;		/* index suppress */
    int   isize;		/* index register size */
    int   iscale;		/* index scale factor */
    int   iis;			/* index/indirect selection */
    int   od = 0;		/* outer displacement */
    int   bsuppress;		/* base suppress */
    char  breg[8];		/* name of base register */
    char  ireg[20];

    switch (mode) {
	case 0:		/* Data Register Direct */
	    sprintf(buf, "D%d", reg);
	    break;
	case 1:		/* Address Register Direct */
	    sprintf(buf, "A%d", reg);
	    break;
	case 2:		/* Address Register Indirect */
	    sprintf(buf, "(A%d)", reg);
	    break;
	case 3:		/* Address Register Indirect with
				 * Postincrement
				 */
	    sprintf(buf, "(A%d)+", reg);
	    break;
	case 4:		/* Address Register Indirect with
				 * Predecrement
				 */
	    sprintf(buf, "-(A%d)", reg);
	    break;
	case 5:		/* Address Register Indirect with
				 * displacement
				 */
	    sprintf(buf, "%d(A%d)", *ip++, reg);
	    break;
	case 6:		/* Address Register and Memory Indirect with
				 * Index
				 */
	    sprintf(breg, "A%d", reg);
	    ext = *ip++;	/* get extension */
	    isize = ((ext >> 11) & 1) + 1;
	    iscale = 1 << ((ext >> 9) & 3);
	    if (ext & 0x100) {	/* Full format */
		switch ((ext >> 4) & 3) {
		    case 0:
		    case 1:
			bd = 0;
			break;
		    case 2:
			bd = *ip++;	/* base displacement is one word */
			break;
		    case 3:
			bd = *uip.l++;
			break;
		}
		isuppress = (ext >> 6) & 1;
		bsuppress = (ext >> 7) & 1;
		iis = ext & 7;
	    }
	    else {		/* Brief format */
		bd = ext & 0xff;
		isuppress = 0;	/* index not suppressed */
		bsuppress = 0;
		iis = 0;
	    }

milab:
	    if (iscale == 1)
		sprintf(ireg, "%s%d.%s", (0x8000 & ext) ? "A" : "D",
			(ext >> 12) & 7, sizes[isize]);
	    else
		sprintf(ireg, "%s%d.%s*%d", (0x8000 & ext) ? "A" : "D",
			(ext >> 12) & 7, sizes[isize], iscale);

	    switch (iis & 3) {
		case 0:
		case 1:
		    od = 0;
		    break;
		case 2:
		    od = *ip++;
		    break;
		case 3:
		    od = *uip.l++;
		    break;
	    }


	    if (isuppress) {
		switch (iis) {
		    case 0:
			if (bsuppress)
			    sprintf(buf, "(%d)", bd);
			else
			    sprintf(buf, "(%d,%s)", bd, breg);
			break;
		    case 1:
			if (bsuppress)
			    sprintf(buf, "([%d])", bd);
			else
			    sprintf(buf, "([%d,%s])", bd, breg);
			break;
		    case 2:
		    case 3:
			if (bsuppress)
			    sprintf(buf, "([%d],%d)", bd, od);
			else
			    sprintf(buf, "([%d,%s],%d)", bd, breg, od);
			break;
		}
	    }
	    else {
		switch (iis) {
		    case 0:
			if (bsuppress)
			    sprintf(buf, "(%d,%s)", bd, ireg);
			else
			    sprintf(buf, "(%d,%s,%s)", bd, breg, ireg);
			break;
		    case 1:
			if (bsuppress)
			    sprintf(buf, "([%d,%s])", bd, ireg);
			else
			    sprintf(buf, "([%d,%s,%s])", bd, breg, ireg);
			break;
		    case 2:
		    case 3:
			if (bsuppress)
			    sprintf(buf, "([%d,%s],%d)", bd, ireg, od);
			else
			    sprintf(buf, "([%d,%s,%s],%d)", bd, breg, ireg, od);
			break;
		    case 4:
			sprintf(buf, "reserved");
			break;
		    case 5:
			if (bsuppress)
			    sprintf(buf, "([%d],%s)", bd, ireg);
			else
			    sprintf(buf, "([%d,%s],%s)", bd, breg, ireg);
			break;
		    case 6:
		    case 7:
			if (bsuppress)
			    sprintf(buf, "([%d],%s,%d)", bd, ireg, od);
			else
			    sprintf(buf, "([%d,%s],%s,%d)", bd, breg, ireg, od);
			break;
		}
	    }
	    break;
	case 7:
	    switch (reg) {
		case 0:	/* Absolute short address */
		    sprintf(buf, "%s", symbolic_name(*ip++));
		    break;
		case 1:	/* Absolute long address */
		    sprintf(buf, "%s", symbolic_name(*uip.l++));
		    break;
		case 2:
		    sprintf(buf, "(%d,PC)", *ip++);
		    break;
		case 3:
		    ext = *ip++;
		    isize = ((ext >> 11) & 1) + 1;
		    iscale = 1 << ((ext >> 9) & 3);
		    if (ext & 0x100) {	/* Full format */
			switch ((ext >> 4) & 3) {
			    case 0:
			    case 1:
				bd = 0;
				break;
			    case 2:
				bd = *ip++;	/* base displacement is one
						 * word
						 */
				break;
			    case 3:
				bd = *uip.l++;
				break;
			}
			isuppress = (ext >> 6) & 1;
			bsuppress = (ext >> 7) & 1;
			iis = ext & 7;
		    }
		    else {	/* Brief format */
			bd = ext & 0xff;
			isuppress = 0;	/* index not suppressed */
			bsuppress = 0;
			iis = 0;
		    }
		    if (isuppress)
			sprintf(breg, "ZPC");
		    else
			sprintf(breg, "PC");
		    goto milab;
		    break;
		case 4:	/* immediate data */
		    switch (size) {
			case 0:
			    ext = *ip++;
			    if (ext & 0x80)
				ext |= 0xffffff00;
			    else
				ext &= 0x7f;
			    sprintf(buf, "#%d", ext);
			    break;
			case 1:
			    sprintf(buf, "#%d", *ip++);
			    break;
			case 2:
			    sprintf(buf, "#%ld", *uip.l++);
			    break;
			default:
			    sprintf(buf, "Bad size");
			    break;
		    }
		    break;
		default:
		    sprintf(buf, "Illegal PC mode");
		    break;
	    }
	    break;
	default:
	    PI_printf("Unknown mode\n");
	    break;
    }
    return buf;
}
