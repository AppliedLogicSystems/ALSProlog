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
int	display_instr	PARAMS( (enum AbstractMachineOps, Code *));


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
/*
#define HOLDPRINTF printf
#define printf PI_printf
*/

static Code *ip;

void
list_asm(addr, n)
    Code *addr;			/* Start address */
    int   n;

{
    Code *stopaddr = addr + n;
    long  ilength;
    enum AbstractMachineOps instr;

	printf("startaddr=%x  stopaddr=%x  codelen=%d\n",addr,stopaddr,n),
	fflush(stdout);

    for (ip = addr; ip < stopaddr; ) {

	instr = decode_instr(*ip);
	if ((instr < 0) || (instr > ICNUM)) 
	{
		printf("[%03.0d]%x: BAD INSTRUCTION: Content=%08.0o\n", (int)(ip-addr), ip, *ip);
		break;
	}
	else
	{
		printf("[%03.0d]%x:",(int)(ip-addr),ip); 
		ilength = display_instr(instr,ip);
		ip += ilength;
		printf("\n");
	}
    }
}

int
display_instr(instr,ip)
    enum AbstractMachineOps instr;
	Code *ip;
{
    long  i, ilength, need_comma;
	ilength = 1;

	printf("%-16s",ic_array[instr].instr_name); 
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
	return(ilength);
}

/*
#undef printf
#define printf HOLDPRINTF
*/

#ifdef TRACEBWAM
void tracewam	PARAMS(( Code * ));

int bwam_trace = 0;

void toggle_bwam PARAMS(( void ));

void
toggle_bwam()
{
	if (bwam_trace == 0)
		bwam_trace = 1;
	else
		bwam_trace = 0;
}

void
tracewam(PP)
    Code *PP;
{
    enum AbstractMachineOps instr;

	if (bwam_trace > 0)
	{
		instr = decode_instr(*PP);
		if ((instr < 0) || (instr > ICNUM)) 
			printf("BAD INSTRUCTION: Content=%08.0o\n", *PP);
		else
		{
			printf("[%x]",PP); 
			display_instr(instr,PP);   
			printf("\n");
		};
		fflush(stdout);
	}
}
#endif
