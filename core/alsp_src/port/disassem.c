/*========================================================================*
 * 				disassem.c           
 *      Copyright (c) 1987-94 Applied Logic Systems, Inc.
 *
 *		-- byte / threaded code disassembler
 *
 * Author: Kevin A. Buettner
 * Creation: 2/17/87 -- for 680x0 architecture
 * Revision History:
 *	5/26/92 : Prabhakaran Raman -- byte code version
 *	9/15/93 : Kevin Buettner -- Threaded code and small improvements
 *========================================================================*/

#include "defs.h"
#include "coerce.h"
#include "wintcode.h"

#define U  0			/* unused  */
#define I  1			/* integer     */
#define D  2			/* displacement */
#define R  3			/* reg  */
#define F  4			/* func, const or int sym */
#define A  5			/* uia */
#define L  6			/* align */
#define T  7			/* table */
#define O  8			/* opsize data */
#define P  9			/* procedure name */
#define S  10			/* skip over (1) */

#define ABMOP(op,p1,p2,p3,p4) {#op,{p1,p2,p3,p4}},

static struct _icode {
    char *instr_name;
    int   arg[4];
} ic_array[] = {

#include "wamops0.h"
};
#undef ABMOP

enum AbstractMachineOps decode_instr PARAMS(( Code ));

/*
 * decode_instr does a linear search to find the instruction.  This is
 * remarkably inefficient for non-threaded code.
 */

enum AbstractMachineOps
decode_instr(inst)
    Code inst;
{
    enum AbstractMachineOps i;
    for (i=0; i<W_NUM_OPS; i++)
	if (abinst(i) == inst)
	    return i;
    return -1;
}


#define ICNUM    sizeof(ic_array)/sizeof(struct _icode)
#define MIN(a,b) ((a)<(b)?(a):(b))
#define printf PI_printf

static Code *ip;

void
list_asm(addr, n)
    Code *addr;			/* Start address */
    int   n;

{
    Code *stopaddr = addr + n;
    long  i, ilength, need_comma;
    enum AbstractMachineOps instr;

	printf("startaddr=%x  stopaddr=%x  codelen=%d\n",addr,stopaddr,n),
	fflush(stdout);

    for (ip = addr; ip < stopaddr; ) {

	instr = decode_instr(*ip);
	ilength = 1;

/*	printf(">%08.0o %08.0o %08.0o %08.0o %08.0o %08.0o %08.0o %08.0o \n",
		*ip,*(ip+1),*(ip+2),*(ip+3),*(ip+4),*(ip+5),*(ip+6),*(ip+7) );
*/

	if ((instr < 0) || (instr > ICNUM)) 
	{
		printf("[%03.0d]%x: BAD INSTRUCTION: Content=%08.0o\n", (int)(ip-addr), ip, *ip);
		break;
	}

/*	printf("[%02.0d]%x[%03.0o|% 3.0d]: %-16s",(int)(ip-addr),ip,instr,instr,ic_array[instr].instr_name); */
	printf("[%02.0d]%x: %-16s",(int)(ip-addr),ip,ic_array[instr].instr_name); 
	fflush(stdout);

#define COMMA if (need_comma++) printf(",\t")

	for (i = 0, need_comma = 0; i < 4; i++)
	    switch (ic_array[instr].arg[i]) {
		case U:	/* Unused */
		    break;

		case I:	/* Integer */
		    COMMA;
		    printf("%ld", *(long *) (ip + ilength));
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case D:	/* Displacement */
		    COMMA;
		    printf("%lx", *(long *) (ip + ilength));
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case R:	/* Register */
		    COMMA;
		    printf("%s(%ld)", (*(ip + ilength) == 0 ? "SP" : "E"),
			   *(long *) (ip + 1 + ilength));
		    ilength += 1 + sizeof (long) / sizeof (Code);
		    break;

		case F:	{ /* Func const or int sym */
		    PWord f = * (PWord *) (ip + ilength);
		    COMMA;
		    if (MTP_CONSTTAG(f) == MTP_SYM) {
			printf("%s",TOKNAME(MFUNCTOR_TOKID(f)));
			if (MFUNCTOR_ARITY(f))
			    printf("/%d",MFUNCTOR_ARITY(f));
		    }
		    else
			printf("%ld",MINTEGER(f));

		    ilength += sizeof (long) / sizeof (Code);
		    break;
		}

		case A:	/* uia */
		    COMMA;
		    printf("'%s'",(char *)(ip+ilength+2));
		    ilength += (*(unsigned long *) (ip + ilength) + 1) * sizeof (long) / sizeof (Code);
		    break;

		case L:	/* aLign */
		    ilength += (sizeof (long) - sizeof (Code)) / sizeof (Code);
		    break;

		case T:	/* Table */
		    COMMA;
		    printf("table");
		    ilength += ((*(long *) (ip + ilength)) * 2 + 1) * sizeof (long) / sizeof (Code);
		    break;

		case O:	/* Opcode size data */
		    COMMA;
		    printf("%ld", *(Code *) (ip + ilength));
		    ilength++;
		    break;
		
		case P: { /* Procedure entry (call and execute instrs) */
		    char buf[1024];
		    COMMA;
		    printf("%s",w_getnamestring((Code *) *(ip + ilength),buf));
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case S:	/* Skip: Padding */
		    ilength += sizeof (long) / sizeof (Code);
		    break;
		
		}

		default:
		    printf("illegal operand\n");
		    als_exit(1);
	    };
	ip += ilength;
	printf("\n");
    }
}
