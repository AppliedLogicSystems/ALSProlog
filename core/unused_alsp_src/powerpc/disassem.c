/*
 * disassem.c		-- disassembler for the IBM RS/6000
 *	Copyright (c) 1992 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 10/1/92
 * Revision History:
 */

#ifdef TESTDISASSEM
#define PI_printf printf
main()
{
    list_asm(main,100);
}
#endif /* TESTDISASSEM */


static long *ip;
list_asm(addr,n)
    long *addr;
    int n;
{
    long *stopaddr = addr+n;
    ip = addr;
    while (ip < stopaddr) {
	PI_printf("%x:\t",(int) ip);
	print_instruction();
    }
}

static print_instruction()
{
    long i;
    i = *ip;
}

/*
 * Operand formats
 */

#define OF_D	1
#define OF_B	2
#define OF_SC	3
#define OF_I	4
#define OF_XL	5
#define OF_XL2	6	/* like XL but with possible "l" 
					appended to instruction name */
#define OF_M	7
#define OF_XO	8
#define OF_XFX	9
#define OF_A	10
#define OF_XFL	11
#define OF_X	12

static struct iinfo {
	char * 	name;
	int	format;
	int	prim_op;
	int	ext_op;
    } itab[] = 
    {
	{ "ti",		O_D,	3,	-1 },	/* trap immediate */
	{ "muli",	O_D,	7,	-1 },	/* multiply immediate */
	{ "sfi",	O_D,	8,	-1 },	/* subtract from immediate */
	{ "dozi",	O_D,	9,	-1 }, /* difference or zero immediate */
	{ "cmpli",	O_D,	10,	-1 },	/* compare logical immediate */
	{ "cmpi",	O_D,	11,	-1 },	/* compare immediate */
	{ "ai",		O_D,	12,	-1 },	/* add immediate */
	{ "ai.",	O_D,	13,	-1 },	/* add immediate and record */
	{ "cal",	O_D,	14,	-1 },	/* compute address lower */
	{ "cau",	O_D,	15,	-1 },	/* compute address upper */
	{ "bc",		O_B,	16,	-1 },	/* branch conditional */
	{ "svc",	O_SC,	17,	-1 },	/* supervisor call */
	{ "b",		O_I,	18,	-1 },	/* branch */
	{ "mcrf",	O_XL,	19,	0  },	/* move condition register field */
	{ "bcr",	O_XL2,	19,	16 },	/* branch conditional register */
	{ "crnor",	O_XL,	19,	33 },	/* condition register NOR */
	{ "crandc",	O_XL,	19,	129},	/* condition register AND with complement */
	{ "crxor",	O_XL,	19,	193},	/* condition register XOR */
	{ "crnand",	O_XL,	19,	225},	/* condition register NAND */
	{ "crand",	O_XL,	19,	257},	/* condition register AND */
	{ "creqv",	O_XL,	19,	289},	/* condition register equivalent */
	{ "crorc",	O_XL,	19,	417},	/* condition register OR with complement */
	{ "cror",	O_XL,	19,	449},	/* condition register OR */
	{ "bcc",	O_XL2,	19,	528},	/* branch conditional to count register */
	{ "rlimi",	O_M,	20,	-1 },	/* rotate left immediate then mask insert */
	{ "rlinm",	O_M,	21,	-1 },	/* rotate left immediate then AND with mask */
	{ "rlmi",	O_M,	22,	-1 },	/* rotate left then mask insert */
	{ "rlnm",	O_M,	23,	-1 },	/* rotate left then AND with mask */
	{ "oril",	O_D,	24,	-1 },	/* OR immediate lower */
	{ "oriu",	O_D,	25,	-1 },	/* OR immediate upper */
	{ "xoril",	O_D,	26,	-1 },	/* XOR immediate lower */
	{ "xoriu",	O_D,	27,	-1 },	/* XOR immediate upper */
	{ "andil",	O_D,	28,	-1 },	/* AND immediate lower */
	{ "andiu",	O_D,	29,	-1 },	/* AND immediate upper */
	{ "cmp",	O_X,	31,	0  },	/* Compare */
	{ "t",		O_X,	31,	4},
	{ "sf",		O_XO,	31,	8},
	{ "a",		O_XO,	31,	10 },	/* Add */
	{ "mfcr",	O_X,	31,	19},
	{ "lx",		O_X,	31,	23},
	{ "sl",		O_X,	31,	24},
	{ "cntlz",	O_X,	31,	26 },	/* Count leading zeros */
	{ "and",	O_X,	31,	28 },	/* AND */
	{ "maskg",	O_X,	31,	29},
	{ "cmpl",	O_X,	31,	32 },	/* Compare logical */
	{ "sfe",	O_XO,	31,	36},
	{ "lux",	O_X,	31,	55},
	{ "andc",	O_X,	31,	60 },	/* AND with complement */
	{ "mfmsr",	O_X,	31,	83},
	{ "lbzx",	O_X,	31,	87},
	{ "neg",	O_XO,	31,	104},
	{ "mul",	O_XO,	31,	107},
	{ "lbzux",	O_X,	31,	119},
	{ "nor",	O_X,	31,	124},
	{ "ae",		O_XO,	31,	138},	/* Add Extended */
	{ "mtcrf",	O_XFX,	31,	144},
	{ "stx",	O_X,	31,	151},
	{ "slq",	O_X,	31,	152},
	{ "sle",	O_X,	31,	153},
	{ "stux",	O_X,	31,	183},
	{ "sliq",	O_X,	31,	184},
	{ "sfze",	O_XO,	31,	200},
	{ "aze",	O_XO,	31,	202},	/* Add to zero extended */
	{ "stbx",	O_X,	31,	215},
	{ "sllq",	O_X,	31,	216},
	{ "sleq",	O_X,	31,	217},
	{ "sfme",	O_XO,	31,	232},
	{ "ame",	O_XO,	31,	234},	/* Add to minus one extended */
	{ "muls",	O_XO,	31,	235},
	{ "stbux",	O_X,	31,	247},
	{ "slliq",	O_X,	31,	248},
	{ "doz",	O_XO,	31,	264},	/* Difference or zero */
	{ "cax",	O_XO,	31,	266},	/* Compute address */
	{ "lscbx",	O_X,	31,	277},
	{ "lhzx",	O_X,	31,	279},
	{ "eqv",	O_X,	31,	284},	/* Equivalent */
	{ "lhzux",	O_X,	31,	311},
	{ "xor",	O_X,	31,	316},
	{ "div",	O_XO,	31,	331},	/* Divide */
	{ "mfspr",	O_X,	31,	339},
	{ "lhax",	O_X,	31,	343},
	{ "abs",	O_XO,	31,	360},	/* Absolute */
	{ "divs",	O_XO,	31,	363},	/* Divide short */
	{ "lhaux",	O_X,	31,	375},
	{ "sthx",	O_X,	31,	407},
	{ "orc"		O_X,	31,	412},
	{ "sthux",	O_X,	31,	439},
	{ "or",		O_X,	31,	444},
	{ "mtspr",	O_X,	31,	467},
	{ "nand",	O_X,	31,	476},
	{ "nabs",	O_XO,	31,	488},
	{ "mcrxr",	O_X,	31,	512},
	{ "sfo",	O_XO,	31,	520},
	{ "ao",		O_XO,	31,	522 },	/* Add */
	{ "lsx",	O_X,	31,	533},
	{ "lbrx",	O_X,	31,	534},	/* Load Byte Reverse Indexed */
	{ "lfsx",	O_X,	31,	535},
	{ "sr",		O_X,	31,	536},
	{ "rrib"	O_X,	31,	537},
	{ "maskir",	O_X,	31,	541},
	{ "sfeo",	O_XO,	31,	548},
	{ "lfsux",	O_X,	31,	567},
	{ "lsi",	O_X,	31,	597},
	{ "lfdx",	O_X,	31,	599},
	{ "nego",	O_XO,	31,	616},
	{ "mulo",	O_XO,	31,	619},
	{ "lfdux",	O_X,	31,	631},
	{ "aeo",	O_XO,	31,	650},	/* Add Extended */
	{ "stsx",	O_X,	31,	661},
	{ "stbrx",	O_X,	31,	662},
	{ "stfsx",	O_X,	31,	663},
	{ "sra",	O_X,	31,	665},
	{ "srq",	O_X,	31,	664},
	{ "stfsux",	O_X,	31,	695},
	{ "sriq",	O_X,	31,	696},
	{ "sfzeo",	O_XO,	31,	712},
	{ "azeo",	O_XO,	31,	714},	/* Add to zero extended */
	{ "stsi",	O_X,	31,	725},
	{ "stfdx",	O_X,	31,	727},
	{ "srlq",	O_X,	31,	728},
	{ "sreq",	O_X,	31,	729},
	{ "sfmeo",	O_XO,	31,	744},
	{ "ameo",	O_XO,	31,	746},	/* Add to minus one extended */
	{ "mulso",	O_XO,	31,	747},
	{ "stfdux",	O_X,	31,	759},
	{ "srliq",	O_X,	31,	760},
	{ "dozo",	O_XO,	31,	776},	/* Difference or zero */
	{ "caxo",	O_XO,	31,	778},	/* Compute address */
	{ "lhbrx",	O_X,	31,	790},
	{ "sra",	O_X,	31,	792},
	{ "srai",	O_X,	31,	824},
	{ "divo",	O_XO,	31,	843},	/* Divide */
	{ "abso",	O_XO,	31,	872},	/* Absolute */
	{ "divso",	O_XO,	31,	875},	/* Divide short */
	{ "sthbrx",	O_X,	31,	918},
	{ "sraq",	O_X,	31,	920},
	{ "srea",	O_X,	31,	921},
	{ "exts",	O_X,	31,	922},	/* Extend sign */
	{ "sraiq",	O_X,	31,	952},
	{ "nabso",	O_XO,	31,	1000},
	{ "l",		O_D,	32,	-1},
	{ "lu",		O_D,	33,	-1},
	{ "lbz",	O_D,	34,	-1},
	{ "lbzu",	O_D,	35,	-1},
	{ "st",		O_D,	36,	-1},
	{ "stu",	O_D,	37,	-1},
	{ "stb",	O_D,	38,	-1},
	{ "stbu",	O_D,	39,	-1},
	{ "lhz",	O_D,	40,	-1},
	{ "lhzu",	O_D,	41,	-1},
	{ "lha",	O_D,	42,	-1},
	{ "lhau",	O_D,	43,	-1},
	{ "sth",	O_D,	44,	-1},
	{ "sthu",	O_D,	45,	-1},
	{ "lm",		O_D,	46,	-1},
	{ "stm",	O_D,	47,	-1},
	{ "lfs",	O_D,	48,	-1},
	{ "lfsu",	O_D,	49,	-1},
	{ "lfd",	O_D,	50,	-1},
	{ "lfdu",	O_D,	51,	-1},
	{ "stfs",	O_D,	52,	-1},
	{ "stfsu",	O_D,	53,	-1},
	{ "stfd",	O_D,	54,	-1},
	{ "stfdu",	O_D,	55,	-1},
	{ "fa",		O_A,	63,	21},
	{ "fabs",	O_X,	63,	264},
	{ "fcmpo",	O_X,	63,	32},
	{ "fcmpu",	O_X,	63,	0},
	{ "fd",		O_A,	63,	8},
	{ "fm",		O_A,	63,	5},
	{ "fma",	O_A,	63,	29},
	{ "fmr",	O_X,	63,	72},
	{ "fms",	O_A,	63,	28},
	{ "fnabs",	O_X,	63,	136},
	{ "fneg",	O_X,	63,	40},
	{ "fnma",	O_A,	63,	31},
	{ "fnms",	O_A,	63,	30},
	{ "frsp",	O_X,	63,	12},
	{ "fs",		O_A,	63,	20},
	{ "mcrfs",	O_X,	63,	64},
	{ "mffs",	O_X,	63,	583},
	{ "mtfsb0",	O_X,	63,	70},
	{ "mtfsb1",	O_X,	63,	38},
	{ "mtfsf",	O_XFL,	63,	711},
	{ "mtfsfi",	O_X,	63,	134}

    };


static int keycmp(key1,key2,t1,t2)
{
    if (key1 < t1)
	return -1;
    else if (key1 > t1)
	return 1;
    else if (key2 == -1)
	return 0;
    else if (key2 < t2)
	return -1;
    else if (key2 > t2)
	return 1;
    else
	return 0;
}


static int bsearch(a,l,h,key1,key2)
    struct iinfo *a;
    int l,h,key1,key2;
{
    int m;
    while (l <= h) {
	m = (l+h) >> 1;
	switch (keycmp(key1,key2,itab[m].prim_op,itab[m].ext_op)) {
	    case -1 :
		h = m-1;
	    case 0 :
		return m;
	    case 1:
		h = l+1;
	}
    }
    return -1;		/* keys not found in table */
}
