/*
 * disassem.c			-- VAX disassembler
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/8/90
 * Revision History:
 */

#ifdef testdisassem
#define PI_printf printf

main()
{
    list_asm(main,100);
}
#endif

typedef unsigned char byte;
static char *symbolic_name();

static byte *ip;

list_asm(addr, n)
    byte *addr;		/* start address */
    int n;		/* number of bytes to disassemble */
{
    byte *stopaddr = addr+n;
    ip = addr;
    while (ip < stopaddr) {
	PI_printf("%x:\t",(int) ip);
	print_instruction();
    }
}


#ifdef notdef
static char *regnames[] = {
	"r0",
	"r1",
	"r2",
	"r3",
	"r4",
	"r5",
	"r6",
	"r7",
	"r8",
	"r9",
	"r10",
	"r11",
	"ap",
	"fp",
	"sp",
	"pc"
};
#endif

static char * regnames[] = {
	"r0",		/* r0 */
	"r1",		/* r1 */
	"r2",		/* r2 */
	"T1",		/* r3 */
	"S",		/* r4 */
	"Safety",	/* r5 */
	"H",		/* r6 */
	"TR",		/* r7 */
	"Fail",		/* r8 */
	"HB",		/* r9 */
	"SPB",		/* r10 */
	"B",		/* r11 */
	"E",		/* r12 */
	"FP",		/* r13 */
	"SP",		/* r14 */
	"pc"		/* r15 */
};

static char EMPTY[] = "";
#define UNKNOWN EMPTY

static struct {
	char *name;
	char *format;
} optab[] =
{
	{ "halt", EMPTY },
	{ "nop",  EMPTY },
	{ "rei", EMPTY },
	{ "bpt", EMPTY },
	{ "ret", EMPTY },
	{ "rsb", EMPTY },
	{ "ldpctx", UNKNOWN },
	{ "svpctx", UNKNOWN },
	{ "cvtps", "rwabrwab" },
	{ "cvtsp", "rwabrwab" },
	{ "index", "rlrlrlrlrlwl" },
	{ "crc", "abrlrwab" },
	{ "prober", EMPTY },
	{ "probew", EMPTY },
	{ "insque", "abab" },
	{ "remque", "abwl" },
	{ "bsbb", "bb" },
	{ "brb", "bb" },
	{ "bneq", "bb" },
	{ "beql", "bb" },
	{ "bgtr", "bb" },
	{ "bleq", "bb" },
	{ "jsb", "ab" },
	{ "jmp", "ab" },
	{ "bgeq", "bb" },
	{ "blss", "bb" },
	{ "bgtru", "bb" },
	{ "blequ", "bb" },
	{ "bvc", "bb" },
	{ "bvs", "bb" },
	{ "bgequ", "bb" },
	{ "blssu", "bb" },
	{ "addp4", "rwabrwab" },
	{ "addp6", "rwabrwabrwab" },
	{ "subp4", "rwabrwab" },
	{ "subp6", "rwabrwabrwab" },
	{ "cvtpt", "rwababrwab" },
	{ "mulp", "rwabrwabrwab" },
	{ "cvttp", "rwababrwab" },
	{ "divp", "rwabrwabrwab" },
	{ "movc3", "rwabab" },
	{ "cmpc3", "rwabab" },
	{ "scanc", "rwababrb" },
	{ "spanc", "rwababrb" },
	{ "movc5", "rwabrbrwab" },
	{ "cmpc5", "rwabrbrwab" },
	{ "movtc", "rwabrbabrwab" },
	{ "movtuc", "rwabrbabrwab" },
	{ "bsbw", "bw" },
	{ "brw", "bw" },
	{ "cvtwl", "rwwl" },
	{ "cvtwb", "rwwb" },
	{ "movp", "rwabab" },
	{ "cmpp3", "rwabab" },
	{ "cvtpl", "rwabwl" },
	{ "cmpp4", "rwabrwab" },
	{ "editpc", "rwababab" },
	{ "matchc", "rwabrwab" },
	{ "locc", "rbrwab" },
	{ "skpc", "rbrwab" },
	{ "movzwl", "rwwl" },
	{ "acbw", "rwrwmwbw" },
	{ "movaw", "awwl" },
	{ "pushaw", "aw" },
	{ "addf2", "rfmf" },
	{ "addf3", "rfrfwf" },
	{ "subf2", "rfmf" },
	{ "subf3", "rfrfwf" },
	{ "mullf2", "rfmf" },
	{ "mullf3", "rfrfwf" },
	{ "divf2", "rfmf" },
	{ "divf3", "rfrfwf" },
	{ "cvtfb", "rfwb" },
	{ "cvtfw", "rfww" },
	{ "cvtfl", "rfwl" },
	{ "cvtrfl",  "rfwl" },
	{ "cvtbf", "rbwf" },
	{ "cvtwf", "rwwf" },
	{ "cvtlf", "rlwf" },
	{ "acbf", "rfrfmfbw" },
	{ "movf", "rfwf" },
	{ "cmpf", "rfrf" },
	{ "mnegf", "rfwf" },
	{ "tstf", "rf" },
	{ "emodf", "rfrbrfwlwf" },
	{ "polyf", "rfrwab" },
	{ "cvtfd", "rfwd" },
	{ "reserved_to_digital", EMPTY },
	{ "adawi", "rwmw" },
	{ "reserved_to_digital", EMPTY },
	{ "reserved_to_digital", EMPTY },
	{ "reserved_to_digital", EMPTY },
	{ "insqhi", "abaq" },
	{ "insqti", "abaq" },
	{ "remqhi", "aqwl" },
	{ "remqti", "aqwl" },
	{ "addd2", "rdmd" },
	{ "addd3", "rdrdwd" },
	{ "subd2", "rdmd" },
	{ "subd3", "rdrdwd" },
	{ "muld2", "rdmd" },
	{ "muld3", "rdrdwd" },
	{ "divd2", "rdmd" },
	{ "divd3", "rdrdwd" },
	{ "cvtdb", "rdwb" },
	{ "cvtdw", "rdww" },
	{ "cvtdl", "rdwl" },
	{ "cvtrdl", "rdwl" },
	{ "cvtbd", "rbwd" },
	{ "cvtwd", "rwwd" },
	{ "cvtld", "rlwd" },
	{ "acbd", "rdrdmdbw" },
	{ "movd", "rdwd" },
	{ "cmpd", "rdrd" },
	{ "mnegd", "rdwd" },
	{ "tstd", "rd" },
	{ "emodd", "rdrbrdwlwd" },
	{ "polyd", "rdrwab" },
	{ "cvtdf", "rdwf" },
	{ "reserved_to_digital", EMPTY },
	{ "ashl", "rbrlwl" },
	{ "ashq", "rbrqwq" },
	{ "emul", "rlrlrlwq" },
	{ "ediv", "rlrqwlwl" },
	{ "clrq", "wq" },
	{ "movq", "rqwq" },
	{ "movaq", "aqwl" },
	{ "pushaq", "aq" },
	{ "addb2", "rbmb" },
	{ "addb3", "rbrbwb" },
	{ "subb2", "rbmb" },
	{ "subb3", "rbrbwb" },
	{ "mulb2", "rbmb" },
	{ "mulb3", "rbrbwb" },
	{ "divb2", "rbmb" },
	{ "divb3", "rbrbwb" },
	{ "bisb2", "rbmb" },
	{ "bisb3", "rbrbwb" },
	{ "bicb2", "rbmb" },
	{ "bicb3", "rbrbwb" },
	{ "xorb2", "rbmb" },
	{ "xorb3", "rbrbwb" },
	{ "mnegb", "rbwb" },
	{ "caseb", "rbrbrbBW" },
	{ "movb", "rbwb" },
	{ "cmpb", "rbrb" },
	{ "mcomb", "rbwb" },
	{ "bitb", "rbrb" },
	{ "clrb", "wb" },
	{ "tstb", "rb" },
	{ "incb", "mb" },
	{ "decb", "mb" },
	{ "cvtbl", "rbwl" },
	{ "cvtbw", "rbww" },
	{ "movzbl", "rbwl" },
	{ "movzbw", "rbww" },
	{ "rotl", "rbrlwl" },
	{ "acbb", "rbrbmbbw" },
	{ "movab", "abwl" },
	{ "pushab", "ab" },
	{ "addw2", "rwmw" },
	{ "addw3", "rwrwww" },
	{ "subw2", "rwmw" },
	{ "subw3", "rwrwww" },
	{ "mulw2", "rwmw" },
	{ "mulw3", "rwrwww" },
	{ "divw2", "rwmw" },
	{ "divw3", "rwrwww" },
	{ "bisw2", "rwmw" },
	{ "bisw3", "rwrwww" },
	{ "bicw2", "rwmw" },
	{ "bicw3", "rwmw" },
	{ "xorw2", "rwmw" },
	{ "xorw3", "rwrwww" },
	{ "mnegw", "rwww" },
	{ "casew", "rwrwrwBW" },
	{ "movw", "rwww" },
	{ "cmpw", "rwrw" },
	{ "mcomw", "rwww" },
	{ "bitw", "rwrw" },
	{ "clrw", "ww" },
	{ "tstw", "rw" },
	{ "incw", "mw" },
	{ "decw", "mw" },
	{ "bispsw", "rw" },
	{ "bicpsw", "rw" },
	{ "popr", "rw" },
	{ "pushr", "rw" },
	{ "chmk", UNKNOWN },
	{ "chme", UNKNOWN },
	{ "chms", UNKNOWN },
	{ "chmu", UNKNOWN },
	{ "addl2", "rlml" },
	{ "addl3", "rlrlwl" },
	{ "subl2", "rlml" },
	{ "subl3", "rlrlwl" },
	{ "mull2", "rlml" },
	{ "mull3", "rlrlwl" },
	{ "divl2", "rlml" },
	{ "divl3", "rlrlwl" },
	{ "bisl2", "rlml" },
	{ "bisl3", "rlrlwl" },
	{ "bicl2", "rlml" },
	{ "bicl3", "rlrlwl" },
	{ "xorl2", "rlml" },
	{ "xorl3", "rlrlwl" },
	{ "mnegl", "rlwl" },
	{ "casel", "rlrlrlBW" },
	{ "movl", "rlwl" },
	{ "cmpl", "rlrl" },
	{ "mcoml", "rlwl" },
	{ "bitl", "rlrl" },
	{ "clrl", "wl" },
	{ "tstl", "rl" },
	{ "incl", "ml" },
	{ "decl", "ml" },
	{ "adwc", "rlml" },
	{ "sbwc", "rlml" },
	{ "mtpr", UNKNOWN },
	{ "mfpr", UNKNOWN },
	{ "movpsl", "wl" },
	{ "pushl", "rl" },
	{ "moval", "alwl" },
	{ "pushal", "al" },
	{ "bbs", "rlabbb" },
	{ "bbc", "rlabbb" },
	{ "bbss", "rlabbb" },
	{ "bbcs", "rlabbb" },
	{ "bbsc", "rlabbb" },
	{ "bbcc", "rlabbb" },
	{ "bbssi", "rlabbb" },
	{ "bbcci", "rlabbb" },
	{ "blbs", "rlbb" },
	{ "blbc", "rlbb" },
	{ "ffs", "rlrbvbwl" },
	{ "ffc", "rlrbvbwl" },
	{ "cmpv", "rlrbvbrl" },
	{ "cmpzv", "rlrbvbrl" },
	{ "extv", "rlrbvbwl" },
	{ "extzv", "rlrbvbwl" },
	{ "insv", "rlrlrbvb" },
	{ "acbl", "rlrlmlbw" },
	{ "aoblss", "rlmlbb" },
	{ "aobleq", "rlmlbb" },
	{ "sobgeq", "mlbb" },
	{ "sobgtr", "mlbb" },
	{ "cvtlb", "rlwb" },
	{ "cvtlw", "rlww" },
	{ "ashp", "rbrwabrbrwab" },
	{ "cvtlp", "rlrwab" },
	{ "callg", "abab" },
	{ "calls", "rlab" },
	{ "xfc", UNKNOWN },
	{ "escd_to_digital", EMPTY },
	{ "esce_to_digital", EMPTY },
	{ "escf_to_digital", EMPTY }
};


static print_instruction()
{
    int i;
    char *format;
    i = *ip++;
    PI_printf("%s",optab[i].name);

    if ((format=optab[i].format) != EMPTY) {
	PI_printf("\t");
	for (;;) {
	    print_operand((int) format[0],(int) format[1]);
	    format += 2;
	    if (*format == '\0')
		break;
	    PI_printf(", ");
	}
    }
    PI_printf("\n");
}

static print_operand(access_type, data_type)
    int access_type;
    int data_type;
{
    if (access_type == 'b') {
	int displ;
	switch (data_type) {
	    case 'b' :
		displ = *ip++;
		if (displ & 0x80)
		    displ |= 0xffffff00;
		print_address(ip+displ);
		break;
	    case 'w' :
		displ = *(short *) ip;
		ip += 2;
		if (displ & 0x8000)
		    displ |= 0xffff0000;
		print_address(ip+displ);
		break;
	    default :
		operand_error(access_type, data_type);
		break;
	}
    }
    else {
	print_general_mode(data_type);
    }
}

static print_address(addr)
    byte *addr;
{
    PI_printf("0x%x",(int) addr);
}


static operand_error(access_type, data_type)
    int access_type;
    int data_type;
{
    PI_printf("Operand error: %c%c",access_type,data_type);
}

static print_general_mode(data_type)
    int data_type;
{
    int s;
    int reg;

    s = *ip++;

    if ((s & 0xc0) == 0) {		/* mode specifiers 0, 1, 2, and 3 */
	PI_printf("#%d",s);
    }
    else {
	reg = s & 0xf;
	if (reg != 15) {
	    switch ((s >> 4) & 0xf) {
		case 5 :		/* Register Mode */
		    PI_printf("%s", regnames[reg]);
		    break;
		case 6 :		/* Register Deferred Mode */
		    PI_printf("(%s)", regnames[reg]);
		    break;
		case 7 :		/* Autodecrement Mode */
		    PI_printf("-(%s)", regnames[reg]);
		    break;
		case 9 :		/* Autoincrement deferred */
		    PI_printf("@");
		    /* fall into Autoincrement mode */
		case 8 :		/* Autoincrement Mode */
		    PI_printf("(%s)+", regnames[reg]);
		    break;
		case 0xb :		/* Byte displacement deferred */
		    PI_printf("@");
		    /* fall into byte displacement */
		case 0xa : 		/* Byte displacement */
		    PI_printf("B^%d(%s)", get_int('b'), regnames[reg]);
		    break;
		case 0xd :		/* Word displacement deferred */
		    PI_printf("@");
		    /* fall into word displacement */
		case 0xc :		/* Word displacement */
		    PI_printf("W^%d(%s)", get_int('w'), regnames[reg]);
		    break;
		case 0xf :		/* Longword displacement deferred */
		    PI_printf("@");
		    /* fall into longword displacement */
		case 0xe :		/* Longword displacement */
		    PI_printf("L^%d(%s)", get_int('l'), regnames[reg]);
		    break;
		case 4 :		/* index mode */
		    print_general_mode(data_type);
		    PI_printf("[%s]", regnames[reg]);
		    break;
	    }
	}
	else {	/* Program Counter Addressing */
	    int disp;
	    switch ((s >> 4) & 0xf) {
		case 4 :		/* index mode */
		    print_general_mode(data_type);
		    PI_printf("[%s]", regnames[reg]);
		    break;
		case 8 :		/* immediate mode */
		    PI_printf("#0x%x", get_int(data_type));
		    break;
		case 9 :		/* absolute mode */
		    PI_printf("@#%s", symbolic_name(get_int('l')));
		    break;
		case 0xb :		/* byte relative deferred */
		    PI_printf("@");
		    /* fall into byte relative */
		case 0xa :		/* byte relative */
		    disp = get_int('b');
		    PI_printf("0x%x",(int) (ip+disp));
		    break;
		case 0xd :		/* word relative deferred */
		    PI_printf("@");
		    /* fall into word relative */
		case 0xc :
		    disp = get_int('w');
		    PI_printf("0x%x",(int) (ip+disp));
		    break;
		case 0xf :
		    PI_printf("@");	/* longword relative deferred */
		    /* fall into longword relative */
		case 0xe :
		    disp = get_int('l');
		    PI_printf("0x%x",(int) (ip+disp));
		    break;
		default :
		    PI_printf("(Illegal PC Mode)");
	    }
	}
    }
}


get_int(dt)
    int dt;
{
    int rv;
    switch (dt) {
	case 'B' :
	case 'b' :
	    rv = *ip++;
	    if (rv & 0x80)
		rv |= 0xffffff00;
	    break;
	case 'w' :
	case 'W' :
	    rv = * (short *) ip;
	    ip += 2;
	    break;
	default :	/* long */
	    rv = * (long *) ip;
	    ip += 4;
	    break;
    }
    return rv;
}


extern wm_g_uia();
extern wm_p_uia();
extern wm_unify();
extern wm_docut();
extern wm_overflow();
extern wm_resolve_ref();
extern wm_execute_builtin();
extern wm_try_me();
extern wm_retry_me();
extern wm_trust_me();
extern wm_g_sym();
extern wm_g_int();
extern wm_u_sym();
extern wm_u_int();
extern wm_u_lval();
extern wm_p_unsafe();
extern wm_docut();
extern wm_fail();


#define LASTUIAINSTR 1

static struct nstruct {
   int (*addr)();
   char *name;
} snames[] =
{
        { wm_g_uia, "wm_g_uia" },
        { wm_p_uia, "wm_p_uia" },       /* This is LASTUIAINSTR */
        { wm_unify, "wm_unify" },
        { wm_docut, "wm_docut" },
        { wm_overflow,  "wm_overflow" },
        { wm_resolve_ref, "wm_resolve_ref" },
        { wm_execute_builtin, "wm_execute_builtin" },
        { wm_try_me,    "wm_try_me" },
        { wm_retry_me,  "wm_retry_me" },
        { wm_trust_me,  "wm_trust_me" },
	{ wm_g_sym,	"wm_g_sym" },
	{ wm_g_int,	"wm_g_int" },
	{ wm_u_sym,	"wm_u_sym" },
	{ wm_u_int,	"wm_u_int" },
	{ wm_u_lval,	"wm_u_lval" },
	{ wm_p_unsafe,	"wm_p_unsafe" },
	{ wm_docut,	"wm_docut" },
        { wm_fail,      "wm_fail" }

};
 
#define BUFSIZE 100
extern char *w_getnamestring();
static char *symbolic_name(addr)
   long *addr;
{
   register int i;
   static char buf[BUFSIZE];
   char *res;
   for (i=0; i < ((sizeof snames) / sizeof (struct nstruct)); i++)
      if ( ((long *) (((long ) snames[i].addr))) == addr ) {
         if (i > LASTUIAINSTR)
            return snames[i].name;
         else {
            sprintf(buf, "%s\n\tUIANAME(%s)", snames[i].name, ip+6);
            ip = ip + 4* (*((short *) ip) & 0xffff) + 6;
            return buf;
         }
      }   
         
   if (res=w_getnamestring(addr, buf))
      return res;
        
   sprintf(buf, "(0x%x)", addr);
   return buf;
}
