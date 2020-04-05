
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
    const char *instr_name;
    int   arg[4];
} ic_array[] = {

#include "wamops0.h"
};
#undef ABMOP

enum AbstractMachineOps decode_instr ( Code );
int	display_instr	(enum AbstractMachineOps, Code *);



#define ICNUM    sizeof(ic_array)/sizeof(struct _icode)
#define MIN(a,b) ((a)<(b)?(a):(b))
/*
#define HOLDPRINTF printf
#define printf PI_printf
*/

static int
relocate_instr(enum AbstractMachineOps instr,Code *ip,prolog_database *db, block_info old_blocks[]);

void
relocate_code(Code *addr, int n, prolog_database *db, block_info old_blocks[])
{
    Code *stopaddr = addr + n, *ip;
    long  ilength;
    enum AbstractMachineOps instr;

#if 0
	//printf("startaddr=%x  stopaddr=%x  codelen=%d\n",
	//	(unsigned int)addr,(unsigned int)stopaddr,n),
	//fflush(stdout);
#endif

    for (ip = addr; ip < stopaddr; ) {

      instr = (enum AbstractMachineOps)*ip; /* always byte code */
      /*instr = (enum AbstractMachineOps)*ip;*/
      if (instr < 0 || instr > ICNUM) 
	{
#if 0
		printf("[%03d]%x: BAD INSTRUCTION: Content=%08o\n",
			(int)(ip-addr), (unsigned int)ip, (unsigned int)*ip);
#endif
		break;
	}
	else
	{
		/*printf("[%03d]%x:",(int)(ip-addr),(unsigned int)ip); */
		ilength = relocate_instr(instr,ip,db,old_blocks);
		ip += ilength;
		/*printf("\n");*/
	}
    }
}

#ifdef Threaded
static int
instr_length(enum AbstractMachineOps instr,Code *ip)
{
    long  i, ilength;
	ilength = 1;

	for (i = 0; i < 4; i++)
	    switch (ic_array[instr].arg[i]) {
		case U:	/* Unused */
		    break;

		case I:	/* Integer */
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case D:	/* Displacement */
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case R:	/* Register */
		    ilength += 1 + sizeof (long) / sizeof (Code);
		    break;

		case F:	{ /* Func const or int sym */
		    ilength += sizeof (long) / sizeof (Code);
		    break;
		}

		case A:	/* uia */
		    ilength += (*(unsigned long *) (ip + ilength) + 1) * sizeof (long) / sizeof (Code);
		    break;

		case L:	/* aLign */
		    ilength += (sizeof (long) - sizeof (Code)) / sizeof (Code);
		    break;

		case T:	/* Table */
		    ilength += ((*(long *) (ip + ilength)) * 2 + 1) * sizeof (long) / sizeof (Code);
		    break;

		case O:	/* Opcode size data */
		    ilength++;
		    break;
		
		case P: { /* Procedure entry (call and execute instrs) */
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

void
thread_byte_convert_clause(Code *addr, int n, prolog_database *db, dirtype direction)
{
    Code *stopaddr = addr + n, *ip;
    long  ilength;
    enum AbstractMachineOps instr;

    for (ip = addr; ip < stopaddr; ) {

      if (direction == convert_byte) {
	instr = decode_instr(*ip);
      } else {
	instr = *ip;
      }
      
      if (instr == W_ILLEGAL_OP || (size_t)instr > ICNUM) 
	{
#if 0
	  printf("[%03d]%x: BAD INSTRUCTION: Content=%08o\n",
	  	(int)(ip-addr), (unsigned int)ip, (unsigned int)*ip);
#endif
		break;
	}
	else
	{
	  if (direction == convert_byte) {
	    *ip = instr;
	  } else {
	    *ip = wam_instrs[instr];
	  }
	  ilength = instr_length(instr,ip);
	  ip += ilength;
	}

    }
}
#endif

static int
relocate_instr(enum AbstractMachineOps instr,Code *ip,prolog_database *db, block_info old_blocks[])
{
    long  i, ilength /*, need_comma*/;
	ilength = 1;

	/*printf("%-16s",ic_array[instr].instr_name); */
	/*fflush(stdout);*/

/* #define COMMA if (need_comma++) printf(",\t") */

#ifdef Threaded
	*ip = wam_instrs[*ip];
#endif

	for (i = 0 /*, need_comma = 0 */; i < 4; i++)
	    switch (ic_array[instr].arg[i]) {
		case U:	/* Unused */
		    break;

		case I:	/* Integer */
		    /*COMMA;*/
		    /*printf("%ld", *(long *) (ip + ilength));*/
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case D:	/* Displacement */
		    /*COMMA;*/
		    fix_offset((void **) (ip + ilength),db,old_blocks);
		    /*printf("%lx", *(long *) (ip + ilength));*/
		    ilength += sizeof (long) / sizeof (Code);
		    break;

		case R:	/* Register */
		    /*//COMMA;*/
		    /*//printf("%s(%ld)", (*(ip + ilength) == 0 ? "SP" : "E"),*/
			/*//   *(long *) (ip + 1 + ilength));*/
		    ilength += 1 + sizeof (long) / sizeof (Code);
		    break;

		case F:	{ /* Func const or int sym */
#if 0
		    PWord f = * (PWord *) (ip + ilength);
		    COMMA;
		    if (MTP_CONSTTAG(f) == MTP_SYM) {
			printf("%s",TOKNAME(MFUNCTOR_TOKID(f)));
			if (MFUNCTOR_ARITY(f))
			    printf("/%d",MFUNCTOR_ARITY(f));
		    }
		    else
			printf("%ld",MINTEGER(f));
#endif
		    ilength += sizeof (long) / sizeof (Code);
		    break;
		}

		case A:	/* uia */
		    /*//COMMA;*/
		    /*//printf("'%s'",(char *)(ip+ilength+2));*/
		    ilength += (*(unsigned long *) (ip + ilength) + 1) * sizeof (long) / sizeof (Code);
		    break;

		case L:	/* aLign */
		    ilength += (sizeof (long) - sizeof (Code)) / sizeof (Code);
		    break;

		case T:	/* Table */
		    /*//COMMA;*/
		  /*//printf("table size: %ld\n", *(ip+1)*2);*/
		    /*// Relocate pointers in table.*/
		    {	
		    	Code *p;
		    	for (p = ip + 3; p < ip+*(ip+1)*2 + 2; p+= 2)
		    		fix_offset((void **)p, db, old_blocks);
		    }
		    ilength += ((*(long *) (ip + ilength)) * 2 + 1) * sizeof (long) / sizeof (Code);
		    break;

		case O:	/* Opcode size data */
		    /*//COMMA;*/
		    /*//printf("%ld", *(Code *) (ip + ilength));*/
		    ilength++;
		    break;
		
		case P: { /* Procedure entry (call and execute instrs) */
		    /*//char buf[1024];*/
		    /*//COMMA;*/
		    fix_offset((void **)(ip + ilength),db,old_blocks);
		    /*//printf("%s",w_getnamestring((Code *) *(ip + ilength),buf));*/
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
