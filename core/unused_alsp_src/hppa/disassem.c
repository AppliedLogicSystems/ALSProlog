/*
 * dissassem.c		-- disassembler for HP9000
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 1-14-93
 * Revision History:
 */

#ifdef TESTDISASSEM
#define PI_printf printf
main()
{
    list_asm((unsigned long *) *(long *)(((long) main) & ~0x3),300);
}
#endif

#define pr printf


static unsigned long *ip;
list_asm(addr,n)
    unsigned long *addr;
    int n;
{
    unsigned long *stopaddr = addr+n;
    ip = addr;
    while (ip < stopaddr) {
	PI_printf("%x:\t",(int) ip);
	print_instruction();
    }
}


/*
 * General Purpose Register Names
 *
 * For the time being, these are %rN where N is an integer between 0 and 31
 * inclusive.  Eventually, we will assign other names to these registers
 * which will have more meaning for our Prolog implementation.
 */

static char *gregs[] = {
	"r0",		/* %r0 */
	"r1",		/* %r1 */
	"rp",		/* %r2 */
	"r3",		/* %r3 */
	"r4",		/* %r4 */
	"r5",		/* %r5 */
	"r6",		/* %r6 */
	"r7",		/* %r7 */
	"r8",		/* %r8 */
	"r9",		/* %r9 */
	"r10",		/* %r10 */
	"r11",		/* %r11 */
	"r12",		/* %r12 */
	"r13",		/* %r13 */
	"r14",		/* %r14 */
	"r15",		/* %r15 */
	"r16",		/* %r16 */
	"r17",		/* %r17 */
	"r18",		/* %r18 */
	"r19",		/* %r19 */
	"r20",		/* %r20 */
	"r21",		/* %r21 */
	"r22",		/* %r22 */
	"arg3",		/* %r23 */
	"arg2",		/* %r24 */
	"arg1",		/* %r25 */
	"arg0",		/* %r26 */
	"dp",		/* %r27 */
	"ret0",		/* %r28 */
	"ret1",		/* %r29 */
	"sp",		/* %r30 */
	"r31"		/* %r31 */
};


/*
 * Space registers
 */

static char *sregs[] = {
	"sr0",		/* %sr0 */
	"sr1",		/* %sr1 */
	"sr2",		/* %sr2 */
	"sr3",		/* %sr3 */
	"sr4",		/* %sr4 */
	"sr5",		/* %sr5 */
	"sr6",		/* %sr6 */
	"sr7",		/* %sr7 */
};


/*
 * Control Registers
 *
 * The symbolic names listed are those found on page 1-9 of the Assembly
 * Language Reference Manual.
 */

static char *cregs[] = {
	"%rctr",	/* %cr0 */
	"%cr1",		/* %cr1 (reserved) */
	"%cr2",		/* %cr2 (reserved) */
	"%cr3",		/* %cr3 (reserved) */
	"%cr4",		/* %cr4 (reserved) */
	"%cr5",		/* %cr5 (reserved) */
	"%cr6",		/* %cr6 (reserved) */
	"%cr7",		/* %cr7 (reserved) */
	"%pidr1",	/* %cr8 */
	"%pidr2",	/* %cr9 */
	"%ccr",		/* %cr10 */
	"%sar",		/* %cr11 */
	"%pidr3",	/* %cr12 */
	"%pidr4",	/* %cr13 */
	"%iva",		/* %cr14 */
	"%eiem",	/* %cr15 */
	"%itmr",	/* %cr16 */
	"%pcsq",	/* %cr17 */
	"%pcoq",	/* %cr18 */
	"%iir",		/* %cr19 */
	"%isr",		/* %cr20 */
	"%ior",		/* %cr21 */
	"%ipsw",	/* %cr22 */
	"%eirr",	/* %cr23 */
	"%ppda",	/* %cr24 */
	"%hta",		/* %cr25 */
	"%tr2",		/* %cr26 */
	"%tr3",		/* %cr27 */
	"%tr4",		/* %cr28 */
	"%tr5",		/* %cr29 */
	"%tr6",		/* %cr30 */
	"%tr7",		/* %cr31 */
};



/*
 * sys_cmplt: Function to call to obtain character string of the opcode
 * completers for the system control instructions.
 */

static char *sys_cmplt(i)
    unsigned long i;
{
    if ((i>>5) & 1)
	return ",M";
    else
	return "";
}

/*
 * sub_cond, cmp_cond: Compare/Subtract Instruction Conditions
 *		(from Table 5-3)
 */

static char *sub_cond_tbl[] = {
			/* c f */
	"",		/* 0 0 */
	",tr",		/* 0 1 */
	",=",		/* 1 0 */
	",<>",		/* 1 1 */
	",<",		/* 2 0 */
	",>=",		/* 2 1 */
	",<=",		/* 3 0 */
	",>",		/* 3 1 */
	",<<",		/* 4 0 */
	",>>=",		/* 4 1 */
	",<<=",		/* 5 0 */
	",>>",		/* 5 1 */
	",sv",		/* 6 0 */
	",nsv",		/* 6 1 */
	",od",		/* 7 0 */
	",ev"		/* 7 1 */
};

static char *sub_cond(i)
    unsigned long i;
{
    return sub_cond_tbl[(i>>12) & 0xf];
}

static char *cmp_cond(i)
    unsigned long i;
{
    return sub_cond_tbl[(i>>12) & 0xe];
}


/*
 * add_cond: Add Instruction Conditions
 *		(from Table 5-4)
 */

static char *add_cond_tbl[] = {
			/* c f */
	"",		/* 0 0 */
	",tr",		/* 0 1 */
	",=",		/* 1 0 */
	",<>",		/* 1 1 */
	",<",		/* 2 0 */
	",>=",		/* 2 1 */
	",<=",		/* 3 0 */
	",>",		/* 3 1 */
	",nuv",		/* 4 0 */
	",uv",		/* 4 1 */
	",znv",		/* 5 0 */
	",vnz",		/* 5 1 */
	",sv",		/* 6 0 */
	",nsv",		/* 6 1 */
	",od",		/* 7 0 */
	",ev"		/* 7 1 */
};

static char *add_cond(i)
    unsigned long i;
{
    return add_cond_tbl[(i>>12) & 0xf];
}


/*
 * log_cond: logical instructions conditions.
 *		(from Table 5-5)
 */

static char *log_cond_tbl[] = {
			/* c f */
	"",		/* 0 0 */
	",tr",		/* 0 1 */
	",=",		/* 1 0 */
	",<>",		/* 1 1 */
	",<",		/* 2 0 */
	",>=",		/* 2 1 */
	",<=",		/* 3 0 */
	",>",		/* 3 1 */
	",??",		/* 4 0 */
	",??",		/* 4 1 */
	",??",		/* 5 0 */
	",??",		/* 5 1 */
	",??",		/* 6 0 */
	",??",		/* 6 1 */
	",od",		/* 7 0 */
	",ev"		/* 7 1 */
};

static char *log_cond(i)
    unsigned long i;
{
    return log_cond_tbl[(i>>12) & 0xf];
}


/*
 * unit_cond: Unit Instruction Conditions
 *		(from Table 5-6)
 */

static char *unit_cond_tbl[] = {
			/* c f */
	"",		/* 0 0 */
	",tr",		/* 0 1 */
	",??",		/* 1 0 */
	",??",		/* 1 1 */
	",sbz",		/* 2 0 */
	",nbz",		/* 2 1 */
	",shz",		/* 3 0 */
	",nhz",		/* 3 1 */
	",sdc",		/* 4 0 */
	",ndc",		/* 4 1 */
	",??",		/* 5 0 */
	",??",		/* 5 1 */
	",sbc",		/* 6 0 */
	",nbc",		/* 6 1 */
	",shc",		/* 7 0 */
	",nhc"		/* 7 1 */
};

static char *unit_cond(i)
    unsigned long i;
{
    return unit_cond_tbl[(i>>12) & 0xf];
}


/*
 * sed_cond: Shift/Extract/Deposit Conditions
 *		(from Table 5-7)
 */

static char *sed_cond_tbl[] = {
	"",
	",=",
	",<",
	",od",
	",tr",
	",<>",
	",>=",
	",ev"
};

static char *sed_cond(i)
    unsigned long i;
{
    return sed_cond_tbl[(i>>13) & 0x7];
}


/*
 * indexed_load_cmplt:
 *	Indexed Load Completers
 *		(from Table 5-8)
 */

static char *indexed_load_cmplt(i)
    unsigned long i;
{
    if (((i>>13)&1) == 0)	/* u bit == 0 */
	if (((i>>5)&1) == 0)	/* m bit == 0 */
	    return "";
	else			/* m bit == 1 */
	    return ",m";
    else			/* u bit == 1 */
	if (((i>>5)&1) == 0)	/* m bit == 0 */
	    return ",s";
	else			/* m bit == 1 */
	    return ",sm";
}

/*
 * short_displ_cmplt:
 *	Short Displacement Load and Store Completers
 *		(from Table 5-9)
 */

static char *short_displ_cmplt(i)
    unsigned long i;
{
    if (((i>>5)&1) == 0)	/* "m" bit == 0 */
	return "";
    else if ((i>>13)&1)		/* "a" bit == 1, "m" bit == 1 */
	return ",mb";
    else			/* "a" bit == 0, "m" bit == 1 */
	return ",ma";
}

/*
 * null_cmplt :
 *	Returns the nullification completer for branch instructions
 */

static char *null_cmplt(i)
    unsigned long i;
{
    return ((i>>1) & 1) ? ",n" : "";
}


/*
 * assemble12:
 *	Based upon definition of assemble_12 on page 5-10.  The instruction
 *	word from a branch instruction is passed in and the signed 12 bit
 *	displacement extracted from this instruction is returned.
 *
 *	The transformation can be represented pictorially as follows (only
 *	twenty-one bits are shown:
 *
 *
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *				|	|
 *				V	V
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|20|20|20|20|20|20|20|20|20|20|18|08|09|10|11|12|13|14|15|16|17|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 */

static long assemble12(i)
    unsigned long i;
{
    return(	((i & 1) ? 0xfffff800 : 0) |
		((i & 4) << 8) |
		((i & 0x1ff8) >> 3)	);
}


/*
 * assemble17:
 *	Based upon definition of assemble_17 on page 5-10.  The instruction
 *	word from a branch instruction is passed in and the signed 17 bit
 *	displacement extracted from this instruction is returned.
 *
 *	The transformation can be represented pictorially as follows (only
 *	twenty-one bits are shown):
 *
 *
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *				|	|
 *				V	V
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|20|20|20|20|20|00|01|02|03|04|18|08|09|10|11|12|13|14|15|16|17|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *
 */

static long assemble17(i)
    unsigned long i;
{
    return(	((i & 1) ? 0xffff0000 : 0) |
		((i & 0x1f0000) >> 5) |
		((i & 4) << 8) |
		((i & 0x1ff8) >> 3)	);
}


/*
 * assemble21:
 *	Assemble a 21 bit constant:  the transformation is shown below
 *	Numbers in the boxes are HP's bit numberings.
 *
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|00|01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *				|	|
 *				V	V
 *	 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *	|20|09|10|11|12|13|14|15|16|17|18|19|05|06|00|01|02|03|04|07|08|
 *	+--+==+==+==+==+--+--+--+--+==+==+==+==+--+--+--+--+==+==+==+==+
 *
 */

static long assemble21(i)
    unsigned long i;
{

    return(	((i & 1) ? 0xfff00000 : 0) |
/*		((i & 0x000001) << 20) | */
		((i & 0x000ffe) << 8) |
		((i & 0x00c000) >> 7) |
		((i & 0x1f0000) >> 14) |
		((i & 0x003000) >> 12)	);
}


static long displ_to_addr(disp)
    long disp;
{
    return ((long) (ip+disp+2));
}



/*
 * spaceid: Return the two bit space id from an instruction word
 */
static char *spaceid_tbl[] = {
	"",
	"sr1,",
	"sr2,",
	"sr3,"
};

static char *spaceid(i)
    unsigned long i;
{
    return spaceid_tbl[(i>>14) & 3];
}


/*
 * spacereg: Return the space register from the 3 bit field of an instruction
 * word
 */

static char *spacereg(i)
    unsigned long i;
{
    return sregs[((i>>11) & 4) | ((i>>14) & 3)];
    /* Screwy, huh? See definition for assemble3 on page 5-10. */
}

/*
 * reg16: return the general purpose register name given by the number found
 * when shifting the instruction word right by 16 bits
 */

static char *reg16(i)
    unsigned long i;
{
    return gregs[(i>>16) & 0x1f];
}

/*
 * reg21: return the general purpose register name given by the number found
 * when shifting the instruction word right by 21 bits
 */

static char *reg21(i)
    unsigned long i;
{
    return gregs[(i>>21) & 0x1f];
}

/*
 * regt: return the target general purpose register
 */

static char *regt(i)
    unsigned long i;
{
    return gregs[i & 0x1f];
}

/*
 * signed_im5
 *
 * This function expects the least significant part of the word passed to
 * it to contain a five bit signed immediate value.  These five bit values
 * (on the HP) store the sign bit in the least significant bit.  This function
 * moves the bits around until we get what we want.
 */

static long signed_im5(i)
    unsigned long i;
{
    return ((i & 1) ? 0xfffffff0 : 0) | ((i>>1) & 0xf);
}

/*
 * signed_im11
 *
 * This function expects the least significant portion of the word passed to
 * it to contain an eleven bit signed immediate value where the low bit
 * designates the sign.  This function extracts this value and puts it into
 * a format we can deal with.
 */

static long signed_im11(i)
    unsigned long i;
{
    return ((i & 1) ? 0xfffffc00 : 0) | ((i>>1) & 0x3ff);
}


/*
 * signed_im14
 *
 * This function expects the least significant portion of the word passed
 * to it to contain a fourteen bit signed immediate value where the low bit
 * designates the sign.  This function extracts this value and puts it into
 * a format we can deal with.
 */

static long signed_im14(i)
    unsigned long i;
{
    return ((i & 1) ? 0xffffe000 : 0) | ((i>>1) & 0x1fff);
}


static char *signed_hex(i)
    long i;
{
    static char buf[80];
    if (i<0)
	sprintf(buf,"-%#x",-i);
    else
	sprintf(buf,"%#x",i);
    return buf;
}



#define MAJOR_OPCODE(op)	(((unsigned int)(op)) >> 26)

static print_instruction()
{
    register unsigned long i;
    char *iname;
    i = *ip;
    switch (MAJOR_OPCODE(i)) {
	case 0x00 :	/* System_op */
	    switch ((i>>5) & 0xff) {
		case 0x00 :	/* BREAK */
		    pr("break\t%#x,%#x",(i&0x1f),((i>>13)&0x1fff));
		    break;
		case 0x20 :	/* SYNC */
		    pr("sync");
		    break;
		case 0x60 :	/* RFI */
		    pr("rfi");
		    break;
		case 0x6B :	/* SSM */
		    iname = "ssm";
l_ssm:		    pr("%s\t%#x, %s", iname, ((i>>16)&0x1f), regt(i));
		    break;
		case 0x73 :	/* RSM */
		    iname ="rsm";
		    goto l_ssm;
		case 0xC3 :	/* MTSM */
		    iname = "mtsm";
		    goto l_ssm;
		case 0x85 :	/* LDSID */
		    pr("ldsid\t(%s%s), %s",	spaceid(i),
						reg21(i),
						regt(i) );
		    break;
		case 0xC1 :	/* MTSP */
		    pr("mtsp\t%s, %s", reg16(i), spacereg(i));
		    break;
		case 0x25 :	/* MFSP */
		    pr("mfsp\t%s, %s", spacereg(i), regt(i));
		    break;
		case 0xC2 :	/* MTCTL */
		    pr("mtctl\t%s, %s",	reg16(i),
					cregs[(i>>21)&0x1f] );
		    break;
		case 0x45 :	/* MFCTL */
		    pr("mfctl\t%s, %s", cregs[(i>>21)&0x1f], regt(i));
		    break;
		default :
		    unrecognized_instruction(i);
		    break;
	    }
	    break;
	case 0x01 :	/* Mem_Mgmt */
	    switch ((i>>6) & 0xff) {
		case 0x46 :	/* PROBER */
		    iname = "prober";
l_prober:	    pr("%s\t(%s%s), %s, %s",	iname,
						spaceid(i),
						reg21(i),
						reg16(i),
						regt(i) );
		    break;
		case 0xC6 :	/* PROBERI */
		    iname = "proberi";
l_proberi:	    pr("%s\t(%s%s), %d, %s",	iname,
						spaceid(i),
						reg21(i),
						reg16(i),
						regt(i) );
		    break;
		case 0x47 :	/* PROBEW */
		    iname = "probew";
		    goto l_prober;
		case 0xC7 :	/* PROBEWI */
		    iname = "probewi";
		    goto l_proberi;
		case 0x4D :	/* LPA */
		    iname = "lpa";
l_lpa:		    pr("%s%s\t%s(%s%s),%s",	iname,
						sys_cmplt(i),
						reg16(i),
						spaceid(i),
						reg21(i),
						regt(i) );
		    break;
		case 0x4C :	/* LHA */
		    iname = "lha";
		    goto l_lpa;
		case 0x48 :	/* PDTLB */
		    iname = "pdtlb";
l_pdtlb:	    pr("%s%s\t%s(%s%s)",	iname,
						sys_cmplt(i),
						reg16(i),
						spaceid(i),
						reg21(i) );
		    break;
		case 0x08 :	/* PITLB */
		case 0x88 :
		    iname = "pitlb";
l_pitlb:	    pr("%s%s\t%s(%s,%s)",	iname,
						sys_cmplt(i),
						reg16(i),
						spacereg(i),
						reg21(i) );
		    break;
		case 0x49 :	/* PDTLBE */
		    iname = "pdtlbe";
		    goto l_pdtlb;
		case 0x09 :	/* PITLBE */
		case 0x89 :
		    iname = "pitlbe";
		    goto l_pitlb;
		case 0x41 :	/* IDTLBA */
		    iname = "idtlba";
l_idtlba:	    pr("%s\t%s, (%s%s)",	iname,
						reg16(i),
						spaceid(i),
						reg21(i) );
		    break;
		case 0x01 :	/* IITLBA */
		case 0x81 :
		    iname = "iitlba";
l_iitlba:	    pr("%s\t%s, (%s,%s)",	iname,
						reg16(i),
						spacereg(i),
						reg21(i) );
		    break;
		case 0x40 :	/* IDTLBP */
		    iname = "idtlbp";
		    goto l_idtlba;
		    break;
		case 0x00 :	/* IITLBP */
		case 0x80 :
		    iname = "iitlbp";
		    goto l_iitlba;
		    break;
		case 0x4E :	/* PDC */
		    iname = "pdc";
		    goto l_pdtlb;
		    break;
		case 0x4A :	/* FDC */
		    iname = "fdc";
		    goto l_pdtlb;
		    break;
		case 0x0A :	/* FIC */
		case 0x8A :
		    iname = "fic";
		    goto l_pitlb;
		    break;
		case 0x4B :	/* FDCE */
		    iname = "fdce";
		    goto l_pdtlb;
		    break;
		case 0x0B :	/* FICE */
		case 0x8B :
		    iname = "fice";
		    goto l_pitlb;
		    break;
		default :
		    unrecognized_instruction(i);
		    break;
	    }
	    break;
	case 0x02 :	/* Arith/Log */
	    switch ((i>>5) & 0x7f) {
		case 0x30 :	/* ADD */
		    iname = "add";
l_add:		    pr("%s%s\t%s, %s, %s",	iname,
						add_cond(i),
						reg16(i),
						reg21(i),
						regt(i) );
		    break;
		case 0x50 :	/* ADDL */
		    iname = "addl";
		    goto l_add;
		case 0x70 :	/* ADDO */
		    iname = "addo";
		    goto l_add;
		case 0x38 :	/* ADDC */
		    iname = "addc";
		    goto l_add;
		case 0x78 :	/* ADDCO */
		    iname = "addco";
		    goto l_add;
		case 0x32 :	/* SH1ADD */
		    iname = "sh1add";
		    goto l_add;
		case 0x52 :	/* SH1ADDL */
		    iname = "sh1addl";
		    goto l_add;
		case 0x72 :	/* SH1ADDO */
		    iname = "sh1addo";
		    goto l_add;
		case 0x34 :	/* SH2ADD */
		    iname = "sh2add";
		    goto l_add;
		case 0x54 :	/* SH2ADDL */
		    iname = "sh2addl";
		    goto l_add;
		case 0x74 :	/* SH2ADDO */
		    iname = "sh2addo";
		    goto l_add;
		case 0x36 :	/* SH3ADD */
		    iname = "sh3add";
		    goto l_add;
		case 0x56 :	/* SH3ADDL */
		    iname = "sh3addl";
		    goto l_add;
		case 0x76 :	/* SH3ADDO */
		    iname = "sh3addo";
		    goto l_add;
		case 0x20 :	/* SUB */
		    iname = "sub";
l_sub:		    pr("%s%s\t%s, %s, %s",	iname,
						sub_cond(i),
						reg16(i),
						reg21(i),
						regt(i) );
		    break;
		case 0x60 :	/* SUBO */
		    iname = "subo";
		    goto l_sub;
		case 0x28 :	/* SUBB */
		    iname = "subb";
		    goto l_sub;
		case 0x68 :	/* SUBBO */
		    iname = "subbo";
		    goto l_sub;
		case 0x26 :	/* SUBT */
		    iname = "subt";
		    goto l_sub;
		case 0x66 :	/* SUBTO */
		    iname = "subto";
		    goto l_sub;
		case 0x22 :	/* DS */
		    iname = "ds";
		    goto l_sub;
		case 0x44 :	/* COMCLR */
		    iname = "comclr";
		    goto l_sub;
		case 0x12 :	/* OR */
		    iname = "or";
l_or:		    pr("%s%s\t%s, %s, %s",	iname,
						log_cond(i),
						reg16(i),
						reg21(i),
						regt(i) );
		    break;
		case 0x14 :	/* XOR */
		    iname = "xor";
		    goto l_or;
		case 0x10 :	/* AND */
		    iname = "and";
		    goto l_or;
		case 0x00 :	/* ANDCM */
		    iname = "andcm";
		    goto l_or;
		case 0x1C :	/* UXOR */
		    iname = "uxor";
l_xor:		    pr("%s%s\t%s, %s, %s",	iname,
						unit_cond(i),
						reg16(i),
						reg21(i),
						regt(i) );
		    break;
		case 0x4C :	/* UADDCM */
		    iname = "uaddcm";
		    goto l_xor;
		case 0x4E :	/* UADDCMT */
		    iname = "uaddcmt";
		    goto l_xor;
		case 0x5C :	/* DCOR */
		    iname = "dcor";
l_dcor:		    pr("%s%s\t%s, %s",		iname,
						unit_cond(i),
						reg21(i),
						regt(i) );
		    break;
		case 0x5E :	/* IDCOR */
		    iname = "idcor";
		    goto l_dcor;
		default :
		    unrecognized_instruction(i);
		    break;
	    }
	    break;
	case 0x03 :	/* Index_mem */
	    if ((i>>12) & 1) 
		switch ((i>>6) & 0xf) {
		    case 0x02 : /* LDWS */
			iname = "ldws";
l_ldws:			pr("%s%s\t%d(%s%s), %s",	iname,
							short_displ_cmplt(i),
							signed_im5(i>>16),
							spaceid(i),
							reg21(i),
							regt(i) );
			break;
		    case 0x01 : /* LDHS */
			iname = "ldhs";
			goto l_ldws;
		    case 0x00 : /* LDBS */
			iname = "ldbs";
			goto l_ldws;
		    case 0x06 : /* LDWAS */
			pr("ldwas%s\t%s(%s), %s",	short_displ_cmplt(i),
							signed_hex(signed_im5(i>>16)),
							reg21(i),
							regt(i) );
			break;
		    case 0x07 : /* LDCWS */
			iname = "ldcws";
			goto l_ldws;
		    case 0x0A : /* STWS */
			iname = "stws";
l_stws:			pr("%s%s\t%s, %x(%s%s)",	iname,
							short_displ_cmplt(i),
							reg16(i),
							signed_hex(signed_im5(i)),
							spaceid(i),
							reg21(i) );
			break;
		    case 0x09 : /* STHS */
			iname = "sths";
			goto l_stws;
		    case 0x08 : /* STBS */
			iname = "stbs";
			goto l_stws;
		    case 0x0E : /* STWAS */
			pr("stwas%s\t%s, %d(%s)",	short_displ_cmplt(i),
							reg16(i),
							signed_im5(i),
							reg21(i) );
			break;
		    case 0x0C : /* STBYS */
			iname = "stbys";
			goto l_stws;
		    default :
			unrecognized_instruction(i);
			break;
		}
	    else
		switch ((i>>6) & 0xf) {
		    case 0x02 :	/* LDWX */
			iname = "ldwx";
l_ldwx:			pr("%s%s\t%s(%s%s), %s",	iname,
							indexed_load_cmplt(i),
							reg16(i),
							spaceid(i),
							reg21(i),
							regt(i) );
			break;
		    case 0x01 :	/* LDHX */
			iname = "ldhx";
			goto l_ldwx;
		    case 0x00 :	/* LDBX */
			iname = "ldbx";
			goto l_ldwx;
		    case 0x06 :	/* LDWAX */
			pr("ldwax%s\t%s(%s), %s",	indexed_load_cmplt(i),
							reg16(i),
							reg21(i),
							regt(i) );
			break;
		    case 0x07 :	/* LDCWX */
			iname = "ldcwx";
			goto l_ldwx;
		    default :
			unrecognized_instruction(i);
			break;
		}
	    break;
	case 0x04 :	/* SPOPn */
	    switch ((i>>9) & 0x3) {
		case 0x00 :	/* SPOP0 */
		    pr("spop0,%d,%#x%s",	(i>>6) & 7,
						((i>>6)&0xfffe0) | (i&0x1f),
						((i>>5) & 1) ? ",n" : "" );
		    break;
		case 0x01 :	/* SPOP1 */
		    pr("spop1,%d,%#x%s\t%s",	(i>>6) & 7,
						(i>>11) & 0x7fff,
						((i>>5) & 1) ? ",n" : "",
						regt(i) );
		    break;
		case 0x02 :	/* SPOP2 */
		    pr("spop2,%d,%#x%s\t%s",	(i>>6) & 7,
						((i>>6)&0x7fe) | (i&0x1f),
						((i>>5) & 1) ? ",n" : "",
						reg21(i) );
		    break;
		case 0x03 :	/* SPOP3 */
		    pr("spop3,%d,%#x%s\t%s, %s",
						(i>>6) & 7,
						((i>>6)&0x3e) | (i&0x1f),
						((i>>5) & 1) ? ",n" : "",
						reg16(i),
						reg21(i) );
		    break;
	    }
	    break;
	case 0x05 :	/* DIAG */
	    pr("diag\t%#x", i&0x3ffffff);
	    break;
	case 0x08 :	/* LDIL */
	    pr("ldil\tL'%s, %s", signed_hex(assemble21(i)<<11), reg21(i));
	    break;
	case 0x09 :	/* Copr_w */
	    if (((i>>12) & 0x1)==0)
		if (((i>>9) & 0x01)==0) {	/* CLDWX */
		    pr("cldwx,%d,%s\t%s(%s%s), %%cpr%d",
						(i>>6) & 7,
						indexed_load_cmplt(i),
						reg16(i),
						spaceid(i),
						reg21(i),
						i & 0x1f);

		}
		else {				/* CSTWX */
		    pr("cstwx,%d,%s\t%%cpr%d, %s(%s%s)",
						(i>>6) & 7,
						indexed_load_cmplt(i),
						i & 0x1f,
						reg16(i),
						spaceid(i),
						reg21(i) );
		}
	    else
		if (((i>>9) & 0x01)==0) {	/* CLDWS */
		    pr("cldws,%d,%s\t%d(%s%s), %%cpr%d",
						(i>>6) & 7,
						short_displ_cmplt(i),
						signed_im5(i>>16),
						spaceid(i),
						reg21(i),
						i & 0x1f);

		}
		else {				/* CSTWS */
		    pr("cstws,%d,%s\t%%cpr%d, %d(%s%s)",
						(i>>6) & 7,
						indexed_load_cmplt(i),
						i & 0x1f,
						signed_im5(i>>16),
						spaceid(i),
						reg21(i) );
		}
	    break;
	case 0x0A :	/* ADDIL */
	    pr("addil\tL'%s, %s", signed_hex(assemble21(i)<<11), reg21(i));
	    break;
	case 0x0B :	/* Copr_dw */
	    if (((i>>12) & 0x1)==0)
		if (((i>>9) & 0x01)==0) {	/* CLDDX */
		    pr("clddx,%d,%s\t%s(%s%s), %%cpr%d",
						(i>>6) & 7,
						indexed_load_cmplt(i),
						reg16(i),
						spaceid(i),
						reg21(i),
						i & 0x1f);
		}
		else {				/* CSTDX */
		    pr("cstdx,%d,%s\t%%cpr%d, %s(%s%s)",
						(i>>6) & 7,
						indexed_load_cmplt(i),
						i & 0x1f,
						reg16(i),
						spaceid(i),
						reg21(i) );
		}
	    else
		if (((i>>9) & 0x01)==0) {	/* CLDDS */
		    pr("cldds,%d,%s\t%d(%s%s), %%cpr%d",
						(i>>6) & 7,
						short_displ_cmplt(i),
						signed_im5(i>>16),
						spaceid(i),
						reg21(i),
						i & 0x1f);
		}
		else {				/* CSTDS */
		    pr("cstds,%d,%s\t%%cpr%d, %d(%s%s)",
						(i>>6) & 7,
						indexed_load_cmplt(i),
						i & 0x1f,
						signed_im5(i>>16),
						spaceid(i),
						reg21(i) );
		}
	    break;
	case 0x0C :	/* COPR */
	    pr("copr,%d,%#x%s",		(i>>6) & 7,
					(i>>4) & 0x3fffe0,
					((i>>5) & 1) ? ",n" : "");
	    break;
	case 0x0D :	/* LDO */
	    pr("ldo\t%s(%s), %s",	signed_hex(signed_im14(i)),
					reg21(i),
					reg16(i) );
	    break;
	case 0x10 :	/* LDB */
	    iname = "ldb";
l_ldb:	    pr("%s\t%s(%s%s), %s",	iname,
					signed_hex(signed_im14(i)),
					spaceid(i),
					reg21(i),
					reg16(i) );
	    break;
	case 0x11 :	/* LDH */
	    iname = "ldh";
	    goto l_ldb;
	case 0x12 :	/* LDW */
	    iname = "ldw";
	    goto l_ldb;
	case 0x13 :	/* LDWM */
	    iname = "ldwm";
	    goto l_ldb;
	case 0x18 :	/* STB */
	    iname = "stb";
l_stb:	    pr("%s\t%s, %s(%s%s)",	iname,
					reg16(i),
					signed_hex(signed_im14(i)),
					spaceid(i),
					reg21(i) );
	    break;
	case 0x19 :	/* STH */
	    iname = "sth";
	    goto l_stb;
	case 0x1A :	/* STW */
	    iname = "stw";
	    goto l_stb;
	case 0x1B :	/* STWM */
	    iname = "stwm";
	    goto l_stb;
	case 0x20 :	/* COMBT */
	    iname = "combt";
l_combt:    pr("%s%s%s\t%s, %s, %#x",	iname,
					cmp_cond(i),
					null_cmplt(i),
					reg16(i),
					reg21(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x21 :	/* COMIBT */
	    iname = "comibt";
l_comibt:   pr("%s%s%s\t%d, %s, %#x",	iname,
					cmp_cond(i),
					null_cmplt(i),
					signed_im5(i>>16),
					reg21(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x22 :	/* COMBF */
	    iname = "combf";
	    goto l_combt;
	case 0x23 :	/* COMIBF */
	    iname = "comibf";
	    goto l_comibt;
	case 0x24 :	/* COMICLR */
	    iname = "comiclr";
l_comiclr:  pr("%s%s\t%d, %s, %s",	iname,
					sub_cond(i),
					signed_im11(i),
					reg21(i),
					reg16(i) );
	    break;
	case 0x25 :	/* Subi */
	    if (((i>>11) & 0x1) == 0) {		/* SUBI */
		iname = "subi";
	    }
	    else {				/* SUBIO */
		iname = "subio";
	    }
	    goto l_comiclr;
	case 0x28 :	/* ADDBT */
	    iname = "addbt";
l_addbt:    pr("%s%s%s\t%s, %s, %#x",	iname,
					add_cond(i&0x700),
					null_cmplt(i),
					reg16(i),
					reg21(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x29 :	/* ADDIBT */
	    iname = "addibt";
l_addibt:   pr("%s%s%s\t%d, %s, %#x",	iname,
					add_cond(i & 0x700),
					null_cmplt(i),
					signed_im5(i>>16),
					reg21(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x2A :	/* ADDBF */
	    iname = "addbf";
	    goto l_addbt;
	case 0x2B :	/* ADDIBF */
	    iname = "addibf";
	    goto l_addibt;
	case 0x2C :	/* Addit */
	    if (((i>>11) & 0x1) == 0) {		/* ADDIT */
		iname = "addit";
	    }
	    else {				/* ADDITO */
		iname = "addito";
	    }
l_addit:    pr("%s%s\t%d, %s, %s",	iname,
					add_cond(i),
					signed_im11(i),
					reg21(i),
					reg16(i) );
	    break;
	case 0x2D :	/* Addi */
	    if (((i>>11) & 0x1) == 0) {		/* ADDI */
		iname = "addi";
	    }
	    else {				/* ADDIO */
		iname = "addio";
	    }
	    goto l_addit;
	case 0x30 :	/* BVB */
	    pr("bvb%s%s\t%s%#x",	sed_cond(i),
					null_cmplt(i),
					reg16(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x31 :	/* BB */
	    pr("bb%s%s\t%s, %d, %#x",	sed_cond(i),
					null_cmplt(i),
					reg16(i),
					(i>>21) & 0x1f,
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x32 :	/* MOVB */
	    pr("movb%s%s\t%s, %s, %#x",	sed_cond(i),
					null_cmplt(i),
					reg16(i),
					reg21(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x33 :	/* MOVIB */
	    pr("movib%s%s\t,%d, %s, %#x",
					sed_cond(i),
					null_cmplt(i),
					signed_im5(i>>16),
					reg21(i),
					displ_to_addr(assemble12(i)) );
	    break;
	case 0x34 :	/* Extract */
	    switch ((i>>10) & 0x7) {
		case 0x00 :	/* VSHD */
		    pr("vshd%s\t%s, %s, %s",
					sed_cond(i),
					reg16(i),
					reg21(i),
					regt(i) );
		    break;
		case 0x02 :	/* SHD */
		    pr("shd%s\t%s, %s, %d, %s",
					sed_cond(i),
					reg16(i),
					reg21(i),
					31-((i>>5)&0x1f),
					regt(i) );
		    break;
		case 0x04 :	/* VEXTRU */
		    iname = "vextru";
l_vextru:	    pr("%s%s\t%s, %d, %s",
					iname,
					sed_cond(i),
					reg21(i),
					32-(i&0x1f),
					reg16(i) );
		    break;
		case 0x05 :	/* VEXTRS */
		    iname = "vextrs";
		    goto l_vextru;
		case 0x06 :	/* EXTRU */
		    iname = "extru";
l_extru:	    pr("%s%s\t%s, %d, %d, %s",
					iname,
					sed_cond(i),
					reg21(i),
					(i>>5)&0x1f,
					32-(i&0x1f),
					reg16(i) );
		    break;
		case 0x07 :	/* EXTRS */
		    iname = "extrs";
		    goto l_extru;
		default :
		    unrecognized_instruction(i);
		    break;
	    }
	    break;
	case 0x35 :	/* Deposit*/
	    switch ((i>>10) & 0x7) {
		case 0x00 :	/* ZVDEP */
		    iname = "zvdep";
l_zvdep:	    pr("%s%s\t%s, %d, %s",
					iname,
					sed_cond(i),
					reg16(i),
					32 - (i&0x1f),
					reg21(i) );
		    break;
		case 0x01 :	/* VDEP */
		    iname = "vdep";
		    goto l_zvdep;
		case 0x02 :	/* ZDEP */
		    iname = "zdep";
l_zdep:		    pr("%s%s\t%s, %d, %d, %s",
					iname,
					sed_cond(i),
					reg16(i),
					31 - ((i>>5) & 0x1f),
					32 - (i & 0x1f),
					reg21(i) );
		    break;
		case 0x03 :	/* DEP */
		    iname = "dep";
		    goto l_zdep;
		case 0x04 :	/* ZVDEPI */
		    iname = "zvdepi";
l_zvdepi:	    pr("%s%s\t%d, %d, %s",
					iname,
					sed_cond(i),
					signed_im5(i>>16),
					32 - (i & 0x1f),
					reg21(i) );
		    break;
		case 0x05 :	/* VDEPI */
		    iname = "vdepi";
		    goto l_zvdepi;
		case 0x06 :	/* ZDEPI */
		    iname = "zdepi";
l_zdepi:	    pr("%s%s\t%d, %d, %d, %s",
					iname,
					sed_cond(i),
					signed_im5(i>>16),
					31 - ((i>>5) & 0x1f),
					32 - (i & 0x1f),
					reg21(i) );
		    break;
		case 0x07 :	/* DEPI */
		    iname = "depi";
		    goto l_zdepi;
	    }
	    break;
	case 0x38 :	/* BE */
	    iname = "be";
l_be:	    pr("%s%s\t%s(%s,%s)",	iname,
					null_cmplt(i),
					signed_hex(assemble17(i)),
					spacereg(i),
					reg21(i) );

	    break;
	case 0x39 :	/* BLE */
	    iname = "ble";
	    goto l_be;
	case 0x3A :	/* Branch */
	    switch ((i>>13) & 0x7) {
		case 0x00 :	/* BL */
		    iname = "bl";
l_bl:		    pr("%s%s\t%#x, %s",	iname,
					null_cmplt(i),
					displ_to_addr(assemble17(i)),
					reg21(i) );
		    break;
		case 0x02 :	/* BLR */
		    pr("blr%s\t%s, %s",	null_cmplt(i),
					reg16(i),
					reg21(i) );
		    break;
		case 0x06 :	/* BV */
		    pr("bv%s\t%s(%s)",	null_cmplt(i),
					reg16(i),
					reg21(i) );
		    break;
		case 0x01 :	/* GATE */
		    iname = "gate";
		    goto l_bl;
		default :
		    unrecognized_instruction(i);
		    break;
	    }
	    break;
	default :
	    unrecognized_instruction(i);
	    break;
    }
    PI_printf("\n");
    ip++;
}

static
unrecognized_instruction(i)
    int i;
{
    PI_printf("Unrecognized instruction: %#x",i);
}
