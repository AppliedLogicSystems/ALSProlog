/*
 * disassem.c		-- 88000 disassembler
 *	Copyright (c) 1988 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 4/26/88
 * Revision History:
 */

#include "defs.h"
#include "module.h"
#undef PI_printf
#define PI_printf printf

static	void	print_instruction	PARAMS(( void ));
static	void	print_mimmed		PARAMS(( int ));
static	void	print_aimmed		PARAMS(( int ));
static	void	print_floating		PARAMS(( int ));
static	long	offset			PARAMS(( long, long ));
static	char *	symbolic_name		PARAMS(( long * ));
static	void	print_rest		PARAMS(( int ));
static	void	print_illegal		PARAMS(( int ));
static	void	print_ibitfield		PARAMS(( int ));
static	void	print_restrest		PARAMS(( int ));

#ifdef notdef
main()
{
   list_asm(main,100);
}
#endif


char *regnames[] = {
	"ZERO",			/* r0 */
	"RET",			/* r1 */
	"A1",			/* r2 */
	"A2",			/* r3 */
	"A3",			/* r4 */
	"S",			/* r5 */
	"T1",			/* r6 */
	"T2",			/* r7 */
	"T3",			/* r8 */
	"T4",			/* r9 */
	"UArg1",		/* r10 */
	"UArg2",		/* r11 */
	"Tmp1",			/* r12 */
	"Tmp2",			/* r13 */
	"OldE",			/* r14 */
	"CP",			/* r15 */
	"TR",			/* r16 */
	"H",			/* r17 */
	"Fail",			/* r18 */
	"SPB",			/* r19 */
	"HB",			/* r20 */
	"B",			/* r21 */
	"Safety",		/* r22 */
	"SP",			/* r23 */
	"HeapBase",		/* r24 */
	"r25",			/* r25 */
	"r26",			/* r26 */
	"r27",			/* r27 */
	"r28",			/* r28 */
	"r29",			/* r29 */
	"E",			/* r30 */
	"CSP"			/* r31 */
};

static long *ip;

void
list_asm(addr,n)
   long *addr;		/* Start address */
   int n;
{
   long *stopaddr = addr+n;
   ip = addr;
   while (ip < stopaddr) {
      PI_printf("%x:\t",(int) ip);
      print_instruction();
   }
}


static void
print_instruction()
{
   long i;
   i = *ip;
   switch ((i>>30) & 0x3) {
      case 0 : print_mimmed(i); break;
      case 1 : print_aimmed(i); break;
      case 2 : print_floating(i); break;
      case 3 : print_rest(i); break;
   }
   ip++;
}

char *memtab[] =
	{
	   "xmem.bu",		/* P = 00, TY = 00 */
	   "xmem",		/* P = 00, TY = 01 */
	   "ld.hu",		/* P = 00, TY = 10 */
	   "ld.bu",		/* P = 00, TY = 11 */
	   "ld.d",		/* P = 01, TY = 00 */
	   "ld",		/* P = 01, TY = 01 */
	   "ld.h",		/* P = 01, TY = 10 */
	   "ld.b",		/* P = 01, TY = 11 */
	   "st.d",		/* P = 10, TY = 00 */
	   "st",		/* P = 10, TY = 01 */
	   "st.h",		/* P = 10, TY = 10 */
	   "st.b",		/* P = 10, TY = 11 */
	   "lda.d",		/* P = 11, TY = 00 */
	   "lda",		/* P = 11, TY = 01 */
	   "lda.h",		/* P = 11, TY = 10 */
	   "lda.b"		/* P = 11, TY = 11 */
	};

#define D(i) (((i)>>21)&0x1f)
#define S1(i) (((i)>>16)&0x1f)
#define LIT16(i) ((i)&0xffff)

static void
print_mimmed(i)
   int i;
{
   PI_printf("%s\t%s, %s, 0x%x\n",memtab[(i>>26)],regnames[D(i)],
			     regnames[S1(i)],LIT16(i));
}

static char *aimmed_tab[]=
	{
	   "and",
	   "and.u",
	   "mask",
	   "mask.u",
	   "xor",
	   "xor.u",
	   "or",
	   "or.u",
	   "addu",
	   "subu",
	   "divu",
	   "mul",
	   "add",
	   "sub",
	   "div",
	   "cmp"
	};

static void
print_aimmed(i)
   int i;
{
   PI_printf("%s\t%s, %s, 0x%x\n",aimmed_tab[((i>>26)&0xf)],regnames[D(i)],
			     regnames[S1(i)],LIT16(i));
}

static void
print_floating(i)
   int i;
{
   PI_printf("Floating pt instruction 0x%x\n",i);
}

static char *btab[] = 
	{
	   "br",
	   "br.n",
	   "bsr",
	   "bsr.n"
	};

static char *bbtab[] =
	{
	   "bb0",
	   "bb0.n",
	   "bb1",
	   "bb1.n"
	};

static char *cmasktab[] =
	{
	   "000",
	   "gt0",
	   "eq0",
	   "ge0",
	   "lt0",
	   "ne0",
	   "le0",
	   "111"
	};


#define B5(i) (((i)>>21)&0x1f)

static char *bitnames[] =
	{
	   "0",		/* 0 */
	   "1",		/* 1 */
	   "EQ",	/* 2 */
	   "NE",	/* 3 */
	   "GT",	/* 4 */
	   "LE",	/* 5 */
	   "LT",	/* 6 */
	   "GE",	/* 7 */
	   "HI",	/* 8 */
	   "LS",	/* 9 */
	   "LO",	/* 10 */
	   "HS",	/* 11 */
	   "12",	/* 12 */
	   "13",	/* 13 */
	   "14",	/* 14 */
	   "15",	/* 15 */
	   "16",	/* 16 */
	   "17",	/* 17 */
	   "18",	/* 18 */
	   "19",	/* 19 */
	   "20",	/* 20 */
	   "21",	/* 21 */
	   "22",	/* 22 */
	   "23",	/* 23 */
	   "24",	/* 24 */
	   "25",	/* 25 */
	   "BSTRC",	/* 26 */
	   "BLIST",	/* 27 */
	   "BUIA",	/* 28 */
	   "BNUM",	/* 29 */
	   "BSYM",	/* 30 */
	   "BREF",	/* 31 */
};

static long
offset(val,n)
    long val;
    long n;
{
   if (val & (1<<(n-1)))
      return (val | (-1 << n));
   else	/* assuming 32 bit integers... */
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
extern	void	wm_resolve_ref0		PARAMS(( void ));
extern	void	wm_resolve_ref1		PARAMS(( void ));
extern	void	wm_resolve_ref2		PARAMS(( void ));
extern	void	wm_resolve_ref3		PARAMS(( void ));
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
	{ wm_resolve_ref0, "wm_resolve_ref0" },
	{ wm_resolve_ref1, "wm_resolve_ref1" },
	{ wm_resolve_ref2, "wm_resolve_ref2" },
	{ wm_resolve_ref3, "wm_resolve_ref3" },
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
	 if (i > LASTUIAINSTR)
	    return snames[i].name;
	 else {
	    sprintf(buf, "%s\n\tUIANAME(%s)", snames[i].name, (char *) (ip+2));
	    ip = ip + (*(ip+1) & 0xffff);
	    return buf;
	 }
      }
   
   if ( (res=w_getnamestring(addr, buf)) )
      return res;
   
   sprintf(buf, "%#lx", (long) addr);
   return buf;
}

static void
print_rest(i)
   int i;
{
   switch ((i>>26)&0xf) {
      case 0 :
      case 1 :
      case 2 :
      case 3 :
	 PI_printf("%s\t%s\n",btab[(i>>26)&0x3],symbolic_name(ip+offset(i,26)));
	 break;
      case 4 :
      case 5 :
      case 6 :
      case 7 :
	 PI_printf("%s\t%s, %s, 0x%lx\n",bbtab[(i>>26)&0x3],bitnames[B5(i)],
				 regnames[S1(i)],(unsigned long)(ip+offset(i,16)));
	 break;
      case 8 :
      case 9 :
      case 0xf :
	 print_illegal(i);
	 break;
      case 0xa :
         PI_printf("bcnd\t%s, %s, 0x%lx\n",cmasktab[((i>>21)&0x7)],
				     regnames[S1(i)],(unsigned long)(ip+offset(i,16)));
	 break;
      case 0xb:
         PI_printf("bcnd.n\t%s, %s, 0x%lx\n",cmasktab[((i>>21)&0x7)],
				       regnames[S1(i)],(unsigned long)(ip+offset(i,16)));
	 break;
      case 0xc :
	 print_ibitfield(i);
	 break;
      case 0xd :
	 print_restrest(i);
	 break;
      case 0xe :
	 PI_printf("tbnd\t%s, 0x%x\n", regnames[S1(i)], LIT16(i));
	 break;
   }
}


static void
print_illegal(i)
   int i;
{
   PI_printf("Illegal Instruction 0x%x\n", i);
}



#define O5(i) ((i)&0x1f)
#define S2(i) ((i)&0x1f)
#define W5(i) (((i)>>5)&0x1f)
#define VEC9(i) ((i)&0x1ff)


static void
print_ibitfield(i)
   int i;
{
   switch ((i>>10)&0x3f) {
      case 0x20 :
	 PI_printf("clr\t%s, %s, %d<%d>\n",regnames[D(i)],
					regnames[S1(i)],W5(i),O5(i));
	 break;
      case 0x24 :
	 PI_printf("ext\t%s, %s, %d<%d>\n",regnames[D(i)],
					regnames[S1(i)],W5(i),O5(i));
	 break;
      case 0x26 :
	 PI_printf("extu\t%s, %s, %d<%d>\n",regnames[D(i)],
					 regnames[S1(i)],W5(i),O5(i));
	 break;
      case 0x28 :
	 PI_printf("mak\t%s, %s, %d<%d>\n",regnames[D(i)],
					regnames[S1(i)],W5(i),O5(i));
	 break;
      case 0x2a :
	 PI_printf("rot\t%s, %s, <%d>\n",regnames[D(i)],
				      regnames[S1(i)],O5(i));
	 break;
      case 0x22 :
	 PI_printf("set\t%s, %s, %d<%d>\n",regnames[D(i)],
					regnames[S1(i)],W5(i),O5(i));
	 break;
      case 0x34 :
	 PI_printf("tb0\t%d, %s, %d\n",B5(i),regnames[S1(i)],VEC9(i));
	 break;
      case 0x36 :
	 PI_printf("tb1\t%d, %s, %d\n",B5(i),regnames[S1(i)],VEC9(i));
	 break;
      case 0x3a :
         PI_printf("tcnd\t%s, %s, %d\n",cmasktab[((i>>21)&3)],
				     regnames[S1(i)],VEC9(i));
	 break;
      default :
	 print_illegal(i);
	 break;
   }
}


char *atab[]=
	{
	   "and",
	   "and.u",
	   "mask",
	   "mask.u",
	   "xor",
	   "xor.u",
	   "or",
	   "or.u",
	   "addu",
	   "subu",
	   "divu",
	   "mul",
	   "add",
	   "sub",
	   "div",
	   "cmp"
	};

char *asuffix[]=
	{
	   "",
	   ".co",
	   ".ci",
	   ".cio"
	};

char *bftab[]=
	{
	   "clr",
	   "set",
	   "ext",
	   "extu",
	   "mak",
	   "rot",
	   "illeg1",
	   "illeg2"
	};

char *jtab[]=
	{
	   "jmp",
	   "jmp.n",
	   "jsr",
	   "jsr.n"
	};

static void
print_restrest(i)
   int i;
{
   switch ((i>>14)&3) {
      case 0 :			/* ld, st, and xmem */
	 if (i & 0x2f)
	    PI_printf("%s\t%s, %s, [%s]\n", memtab[((i>>10)&0xf)],
		   regnames[D(i)], regnames[S1(i)], regnames[S2(i)]);
	 else
	    PI_printf("%s\t%s, %s, %s\n", memtab[((i>>10)&0xf)], 
		   regnames[D(i)], regnames[S1(i)], regnames[S2(i)]);

	 break;
      case 1 :			/* logical and arithmetic ops */
	 PI_printf("%s%s\t%s, %s, %s\n", atab[((i>>10)&0xf)], 
		asuffix[((i>>8)&3)], regnames[D(i)], regnames[S1(i)], 
		regnames[S2(i)]);
	 break;
      case 2 :			/* bit field operations */
	 PI_printf("%s\t%s, %s, %s\n", bftab[((i>>11)&0x7)], 
		regnames[D(i)], regnames[S1(i)], regnames[S2(i)]);
	 break;
      case 3 :			/* control */
	 switch((i>>10)&0xf) {
	    case 0x0 :
	    case 0x1 :
	    case 0x2 :
	    case 0x3 :
	       PI_printf("%s\t%s\n", jtab[((i>>10)&3)], regnames[S2(i)]);
	       break;
	    case 0xa :
	       PI_printf("ff1\t%s, %s\n", regnames[D(i)], regnames[S2(i)]);
	       break;
	    case 0xb :
	       PI_printf("ff0\t%s, %s\n", regnames[D(i)], regnames[S2(i)]);
	       break;
	    case 0xe :
	       PI_printf("tbnd\t%s, %s\n", regnames[S1(i)], regnames[S2(i)]);
	       break;
	    case 0xf :
	       PI_printf("rte\n");
	       break;
	    default :
	       print_illegal(i);
	       break;
	 }
	 break;
    }
}
