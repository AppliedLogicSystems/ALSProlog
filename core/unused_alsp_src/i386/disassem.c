/*
 * disassem.c   -- Disassembler for 386 System 
 *
 * Copyright (c) 1987 Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 4/20/89  
 */

#include "config.h"

#include <stdio.h>

#include "types.h"
#include "coerce.h"



/*
#define DISASSEM_DEBUG 	1
*/


/*
 * Global variables used by the disassembler.
 */
static Code *ip;      		/* instruction pointer */
static int opcode;          /* opcode */
static int w;               /* byte or full size (32-bit) data */
static int mod, reg, rm;    /* subfields of MODRM field */
static int ss, basereg, inxreg;    /* subfields of 2nd byte of MODRM field */



/*
 * Global variables can be accessed by Prolog code 
 */

typedef struct {
   char  *varname;
   long  varaddr;
} disassem_varaddr_struct;

extern PWord UnifyPtr;
extern PWord OverflowPtr;
extern PWord swConstPtr;
extern PWord swStructPtr;
extern PWord wm_b_reg;
extern PWord wm_safety;
extern PWord wm_heapbase;

#undef PROC
#define PROC(procname,addr)    {procname,(long)addr}, 

static disassem_varaddr_struct disassem_vars[] = {
   		PROC("OverflowPtr", &OverflowPtr)
   		PROC("wm_safety", &wm_safety)
   		PROC("wm_b_reg", &wm_b_reg)
   		PROC("UnifyPtr", &UnifyPtr)
   		PROC("swConstPtr", &swConstPtr)
   		PROC("swStructPtr", &swStructPtr)
   		PROC("wm_heapbase",&wm_heapbase)
};

#undef PROC

#define NUMVARADDR (sizeof(disassem_vars) / sizeof(disassem_varaddr_struct))



extern wm_try_me();
extern wm_retry_me();
extern wm_p_uia();
extern wm_g_uia();
extern call_mod_closure();
extern wm_execute_builtin();



#define INDEXPROC(addr) (addr == (long)wm_try_me || addr == (long)wm_retry_me)

#define UIAPROC(addr) (addr == (long)wm_p_uia || addr == (long)wm_g_uia)

#define DISASSEM(function) if (function() != 0) return(1); 



/*
 * Disassember
 */
list_asm(addr,n)
	Code *addr;		/* Start address */
	int n;			/* size of code */
{
	Code *stopaddr;
	Code *inststart;
	int  i;

	/*
	 * Disassembly instructions.
	 */
	stopaddr  = (Code *)addr+n;
	ip = (Code *)addr;
	while (ip < stopaddr) {
		inststart = ip;
		if (disassem_instruction() == -1) {
			ip = inststart;
			break;
		}
#ifdef DISASSEM_DEBUG
		PI_printf("\t");
		while (inststart < ip)  
			PI_printf("%02x ", (*inststart++ & 0xff));
#endif
	}
	/*
	 * Print the code which is not disassemblied. 
	 */
	if (ip < stopaddr) {
		for(i=0; ip < stopaddr; i++) {
			if ((i%16) == 0)
				PI_printf("\n%lx:    ", ip);
			PI_printf("%02x ", (*ip++ & 0xff));
		}
	}
}



/*
 * Disassembly the current instruction.
 */
static disassem_instruction()
{
	PI_printf("\n%lx:    ", ip);
	opcode = (*ip++) & 0xff;		/* get out opcode */

	DISASSEM(disassem_move)
	DISASSEM(disassem_jump_on_condition)
	DISASSEM(disassem_arith_logic)
	DISASSEM(disassem_group1)
	DISASSEM(disassem_group2)
	DISASSEM(disassem_group3)
	DISASSEM(disassem_group4)
	DISASSEM(disassem_group5)
	DISASSEM(disassem_inc_dec_reg)
	DISASSEM(disassem_push_pop)
	DISASSEM(disassem_misc)

	PI_printf("Unable to disassembly opcode: %02x ", opcode);
	return(-1);
}


/*
 * Buffers to hold operands of instructions
 */
static char resolve_buf[64];
static char modrm_buf[64];



static char *resolveaddr(addr)
	long addr;
{
    int   i;
	char  *bltname;

   	/*
     * check public variables 
     */
   	for(i=0; i<NUMVARADDR; i++) 
       	if (addr == disassem_vars[i].varaddr) 
           	return(disassem_vars[i].varname);

	if ((bltname=(char *)is_builtin_name(addr)) != (char *) -1) 
		return(bltname);

   	/*
     * Unresolved 
     */
	sprintf(resolve_buf,"%08lx",addr);
   	return(resolve_buf);
}




static decode_modrm() 
{
	char modrm = *ip++;

	mod = (modrm >> 6) & 0x3;
	reg = (modrm >> 3) & 0x7;
	rm = modrm & 0x7;
}

static decode_modrm2() 
{
	char modrm = *ip++;

	ss      = (modrm >> 6) & 0x3;
	inxreg  = (modrm >> 3) & 0x7;
	basereg = modrm & 0x7;
}




static char *LONGREGNAMES[] = {"EAX","ECX","EDX","EBX","ESP","EBP","ESI","EDI"};
static char *BYTEREGNAMES[] = {"AL","CL","DL","BL","AH","CH","DH","BH"};
static int  SFACTORS[] = {1,2,4,8};

static char *REGNAMES(r)
	int r;
{
	if (w)
		return(LONGREGNAMES[r]);
	else
		return(BYTEREGNAMES[r]);
}



static long decode_immedval()
{
    if (w)  /* 32-bit immediate */
       return(*(LongPtr(ip))++);
    else    /* 8-bit immediate */
       return((long)*ip++);
}




static char *EA2()
{
    decode_modrm2();

    switch (mod) {
        case 0:
          	if (basereg == 5) {
				if (inxreg == 4) 	/* ea: [disp32] */
			 		sprintf(modrm_buf,"[%s]", resolveaddr(*(LongPtr(ip))++));
				else  	/* ea: [disp32+(inxreg*scalefactor)] */
			 		sprintf(modrm_buf,"[%s+(%s*%d)]", 
							resolveaddr(*(LongPtr(ip))++),
							LONGREGNAMES[inxreg],SFACTORS[ss]);
			}
			else {
				if (inxreg == 4)  	/* ea: [basereg] */
			 		sprintf(modrm_buf,"[%s]", LONGREGNAMES[basereg]);
				else  	/* ea: [basereg+(inxreg*scalefactor)] */
			 		sprintf(modrm_buf,"[%s+(%s*%d)]", 
							LONGREGNAMES[basereg],
							LONGREGNAMES[inxreg],SFACTORS[ss]);
			}
			return(modrm_buf);
          	break;
       	case 1:
			if (inxreg == 4)  	/* ea: [basereg+disp8] */
				sprintf(modrm_buf,"[%s+%02x]", 
						LONGREGNAMES[basereg],(*ip++&0xff));
			else  	/* ea: [basereg+(inxreg*scalefactor)+disp8] */
				sprintf(modrm_buf,"[%s+(%s*%d)+%02x]", 
						LONGREGNAMES[basereg],
						LONGREGNAMES[inxreg],SFACTORS[ss],(*ip++&0xff));
			return(modrm_buf);
          	break;
       	case 2:
			if (inxreg == 4)  	/* ea: [basereg+disp32] */
				sprintf(modrm_buf,"[%s+%s]", 
						LONGREGNAMES[basereg],resolveaddr(*(LongPtr(ip))++));
			else  	/* ea: [basereg+(inxreg*scalefactor)+disp32] */
				sprintf(modrm_buf,"[%s+(%s*%d)+%s]", 
						LONGREGNAMES[basereg], 
						LONGREGNAMES[inxreg],SFACTORS[ss],
						resolveaddr(*(LongPtr(ip))++));
			return(modrm_buf);
          	break;
		defult:
			fprintf(stderr,"\nInternal Error: Illegal mod field in EA2.");
			break;
	}
}
 


static char *EA()
{
	switch (mod) {
	   	case 0:    
          	if (rm == 4)  /* escape to two-byte MODRM */
		     	return(EA2());
          	else if (rm == 5) /* ea: disp32 */
		  		return(resolveaddr(*(LongPtr(ip))++));
			else { 	/* ea : [rm]  */
			 	sprintf(modrm_buf,"[%s]",LONGREGNAMES[rm]);
			 	return(modrm_buf);
			}
          	break;
	   	case 1:    
          	if (rm == 4)  /* escape to two-byte MODRM */
		     	return(EA2());
          	else { 	/* ea: [rm+disp8] */
			 	sprintf(modrm_buf,"[%s+%02x]",LONGREGNAMES[rm],(*ip++&0xff));
			 	return(modrm_buf);
			}
          	break;
	   	case 2:    
          	if (rm == 4)  /* escape to two-byte MODRM */
		     	return(EA2());
          	else {	/* ea: [reg+disp32] */
			 	sprintf(modrm_buf,"[%s+%s]",
						LONGREGNAMES[rm],resolveaddr(*(LongPtr(ip))++));
			 	return(modrm_buf);
			}
          	break;
	   	case 3:    
          	/* ea: rm */
			return(REGNAMES(rm));
          	break;
		defult:
			fprintf(stderr,"\nInternal Error: Illegal mod field in EA.");
			break;
    }  
}




	

static disassem_move()
{
    if ((opcode & 0xfc) == 0x88) {
	   	w = opcode & 0x01; 
       	decode_modrm();
		if ((opcode & 0x02) == 0) 
			PI_printf("%-7s %s, %s", "MOV ", EA(), REGNAMES(reg));
		else
			PI_printf("%-7s %s, %s", "MOV ", REGNAMES(reg), EA());
    }
    else if (((opcode & 0xf0) == 0xb0) ||
             (((opcode & 0xfe) == 0xc6) && ((*ip & 0x38) == 0x00))) {   
		char *dest;
		long moveimmreg;
		long immedval;
       	if ((opcode & 0xf0) == 0xb0) {  
          	w = opcode & 0x08;  
          	moveimmreg  = opcode & 0x07; 
			dest = REGNAMES(moveimmreg);
       	}
       	else {  
          	w = opcode & 0x01;  
          	decode_modrm();
          	dest = EA();
          	if (mod == 3)
             	moveimmreg = rm ;
           else
             	moveimmreg = -1;
       	}
       	immedval = decode_immedval();
       	if (w && (moveimmreg >= 0)) {
        	/* 
			 * Instruction is "mov reg,immedval(32-bit)"
			 */
			if (((*ip & 0xff) == 0xff) && 
		   		((*(ip+1) & 0xff) == (0xd0 | (moveimmreg & 0x03)))) {
          		/*
           	 	 * Next instruction is "call reg", 
				 */
				PI_printf("%-7s %s, %s", "MOV ", dest, resolveaddr(immedval));
				PI_printf("\n%lx:    ", ip);
				PI_printf("%-7s %s", "CALL", REGNAMES(moveimmreg));
				ip += 2; 	/* skip call instruction */
          		if (INDEXPROC(immedval)) {
             		/*
              		 * The immediate value in the previous "mov" instruction
              		 * is the address of procedure "wm_try_me" or "wm_retry_me",
              		 * and the current instruction is "call reg".
              		 * So, the next longword is the next clause address.
              		 */
					PI_printf("\n%lx:    ", ip);
					PI_printf("Next Clause: %s",resolveaddr(*(LongPtr(ip))++));
          		}
          		else if (UIAPROC(immedval)) {
                  	/*
				   	 * The immediate value in the previous "mov" instruction
				   	 * is the address of procedure "wm_p_uia" or "wm_g_uia",
				   	 * and the current instruction is "call reg".
				   	 * So, the current instruction is followed by an uia string.
				   	 * We have to skip that uia string.
				   	 * Since the length of following uia string is stored in 
				   	 * the first long word, we have to skip that long word and
				   	 * actual uia string.
				   	 */
					int uiasize,i;
					PI_printf("\n%lx:    ", ip);
					uiasize = *(LongPtr(ip))++;
					PI_printf("UIA: %08lx ", uiasize);
					for (i=1; uiasize--; i++) {
						if ((i%4) == 0)
							PI_printf("\n%lx:         ", ip);
						PI_printf("%08lx ", *(LongPtr(ip))++);
					}
               	}
				return(1);
       		}
			else if (((*ip & 0xff) == 0xff) && 
		   		((*(ip+1) & 0xff) == (0xe0 | (moveimmreg & 0x03)))) {
          		/*
           	 	 * Next instruction is "jmp reg", 
				 */
				PI_printf("%-7s %s, %s", "MOV ", dest, resolveaddr(immedval));
				PI_printf("\n%lx:    ", ip);
				PI_printf("%-7s %s", "JMP ", REGNAMES(moveimmreg));
				ip += 2; 	/* skip jmp instruction */
				return(1);
			}
			else if (((*ip & 0xf8) == 0xb8) &&
			  		 ((*(long *)(ip+1) == (long)call_mod_closure) ||
			    	  (*(long *)(ip+1) == (long)wm_execute_builtin))) {
          		/*
           	 	 * Next instruction is "mov reg,call_mod_closure" or
           	 	 *                     "mov reg,wm_execute_builtin" 
				 */
				PI_printf("%-7s %s, %s", "MOV ", dest, resolveaddr(immedval));
				PI_printf("\n%lx:    ", ip);
				PI_printf("%-7s %s, %s", "MOV ", LONGREGNAMES[(*ip&0x07)],
						resolveaddr(*((long *)(ip+1))));
				ip += 5; 	/* skip mov instruction */
				return(1);
			}
			else {
          		/*
           	 	 * Next instruction is not a special instruction
				 */
				PI_printf("%-7s %s, %lx", "MOV ", dest, immedval);
				return(1);
			}
		}
		else {
			PI_printf("%-7s %s, %lx", "MOV ", dest, immedval);
			return(1);
		}
	}	/* End of Move Immediate */
	else
		return(0);

    return(1);
}




static char *JccNAMES[] =
		{"JO  ", "JNO ", "JB  ", "JNB ", "JZ  ", "JNZ ", "JBE ", "JNBE",
 		 "JS  ", "JNS ", "JP  ", "JNP ", "JL  ", "JNL ", "JLE ", "JNLE"}; 

static disassem_jump_on_condition()
{
	int 	jcc;
	long 	disp;
    Code 	*jmpaddr;

    if ((opcode & 0xf0) == 0x70) {   /* 8-bit displacement */
		jcc = (int) (opcode & 0x0f);
	   	disp = (long)*ip++;
    }
    else if ((opcode == 0x0f) && 
             ((*ip & 0xf0) == 0x80)) {   /* 32-bit displacement */
		jcc = (int) (*ip & 0x0f);
       	ip++;  /* skip 2nd byte of opcode */
	   	disp = *(LongPtr(ip))++;
    }  
    else
       	return(0);

	jmpaddr   = ip + disp;
	PI_printf("%-7s %lx", JccNAMES[jcc], jmpaddr);
	return(1);
}




static char *ArithLogicNAMES[] = 
		{"ADD ", "OR  ", "ADC ", "SBB ", "AND ", "SUB ", "XOR ", "CMP "};

static disassem_arith_logic()
{
    if ((opcode & 0xc4) == 0x00) {
	   	w = opcode & 0x01; 
       	decode_modrm();
		if ((opcode & 0x02) == 0) 
			PI_printf("%-7s %s, %s", ArithLogicNAMES[((opcode&0x38)>>3)], 
					EA(),REGNAMES(reg));
		else
			PI_printf("%-7s %s, %s", ArithLogicNAMES[((opcode&0x38)>>3)], 
					REGNAMES(reg),EA());
    }
    else if ((opcode & 0xc6) == 0x04) {
	   	w = opcode & 0x01; 
		PI_printf("%-7s %s, %lx", ArithLogicNAMES[((opcode&0x38)>>3)], 
				REGNAMES(0),decode_immedval());
    }  
    else 
       	return(0);

    return(1);
}




static disassem_group1()
{
    if ((opcode & 0xfe) == 0x80) {
	   	w = opcode & 0x01; 
       	decode_modrm();
		PI_printf("%-7s %s, %lx", ArithLogicNAMES[reg], 
				EA(),decode_immedval());
    }
    else if (opcode == 0x83) {
		char *Ev;
		long Ib;
       	decode_modrm();
	   	w = 1; Ev = EA();
		w = 0; Ib = decode_immedval();
		PI_printf("%-7s %s, %lx", ArithLogicNAMES[reg],Ev,Ib); 
    }
	else
       	return(0);

    return(1);
}




static char *Group2NAMES[] =
		{"ROL ", "ROR ", "RCL ", "RCR ", "SHL ", "SHR ", "ILL ", "SAR "};

static disassem_group2()
{
	if (((opcode & 0xfc) == 0xd0) || ((opcode & 0xfe) == 0xc0)) {
		w = (opcode & 0x01);
		decode_modrm();
		if (reg == 6)
			return(0);
		else if ((opcode & 0xfe) == 0xc0) {
			char *Evb;
			long Ib;
			Evb = EA();
			w = 0; Ib = decode_immedval();
			PI_printf("%-7s %s, %lx", Group2NAMES[reg],Evb,Ib);
		}
		else if ((opcode & 0xfd) == 0xd0)
			PI_printf("%-7s %s, %lx", Group2NAMES[reg],EA(),1);
		else
			PI_printf("%-7s %s, %s", Group2NAMES[reg],EA(),"CL");
	}
	else 
		return(0);

	return(1);
}




static char *Group3NAMES[] =
		{"TEST", "ILL ", "NOT ", "NEG ", "MUL ", "IMUL", "DIV ", "IDIV"};

static disassem_group3()
{
	if ((opcode & 0xfe) == 0xf6) {
		w = (opcode & 0x01);
		decode_modrm();
		if (reg == 1)
			return(0);
		else if (reg == 0)
			PI_printf("%-7s %s, %lx", Group3NAMES[reg],EA(),decode_immedval());
		else if (reg > 3)
			PI_printf("%-7s %s, %s", Group3NAMES[reg],EA(),REGNAMES(0));
		else
			PI_printf("%-7s %s", Group3NAMES[reg],EA());
	}
	else 
		return(0);
	
	return(1);
}




static char *Group4NAMES[] =
		{"INC ", "DEC ", "ILL ", "ILL ", "ILL ", "ILL ", "ILL ", "ILL"};

static disassem_group4()
{
	if (opcode == 0xfe) {
		w = 0;
		decode_modrm();
		if (reg > 1)
			return(0);
		else
			PI_printf("%-7s %s", Group4NAMES[reg], EA());
	}
	else
		return(0);

	return(1);
}




static char *Group5NAMES[] =
		{"INC ", "DEC ", "CALL", "ILL ", "JMP ", "ILL ", "PUSH", "ILL"};

static disassem_group5()
{
	if (opcode == 0xff) {
		w = 1;
		decode_modrm();
		if (reg == 3 || reg == 5 || reg == 7)
			return(0);
		else
			PI_printf("%-7s %s", Group5NAMES[reg], EA());
	}
	else
		return(0);

	return(1);
}
	



static disassem_inc_dec_reg()
{
    if ((opcode & 0xf8) == 0x40)  
		PI_printf("%-7s %s", "INC ", LONGREGNAMES[(opcode&0x07)]);
    else if ((opcode & 0xf8) == 0x48)  
		PI_printf("%-7s %s", "DEC ", LONGREGNAMES[(opcode&0x07)]);
	else 
		return(0);

	return(1);
}




static disassem_push_pop()
{
    if ((opcode & 0xf8) == 0x50)  
		PI_printf("%-7s %s", "PUSH", LONGREGNAMES[(opcode&0x07)]);
    else if ((opcode & 0xf8) == 0x58) 
		PI_printf("%-7s %s", "POP ", LONGREGNAMES[(opcode&0x07)]);
	else if ((opcode & 0xfd) == 0x68) { 
	   	if ((opcode & 0x02) == 0x00)
			w = 1;
		else 
			w = 0;
		PI_printf("%-7s %lx", "PUSH", decode_immedval());
	} 
	else if ((opcode == 0x8f) && ((*ip & 0x38) == 0x00)) {
		w = 1;
		decode_modrm();
		PI_printf("%-7s %s", "POP ", EA());
	}
	else if (opcode == 0x60)
		PI_printf("%-7s", "PUSHAD");
	else if (opcode == 0x61)
		PI_printf("%-7s", "POPAD");
	else 
	   	return(0);

	return(1);
}




static disassem_misc()
{
    if (opcode == 0x8d) {  /* lea */
		w = 1;
		decode_modrm();
		PI_printf("%-7s %s, %s", "LEA ", REGNAMES(reg), EA());
    }
	else if ((opcode & 0xfd) == 0xe9) { /* jmp */
		long 	disp;
    	if ((opcode & 0x02) == 0x00)   
	   		disp = *(LongPtr(ip))++;
    	else 
	   		disp = (long)*ip++;
		PI_printf("%-7s %s", "JMP ", resolveaddr(ip+disp));
	}
	else if ((opcode & 0xf6) == 0xc2) {  /* ret */
		if ((opcode & 0x01) == 0x00) 
			PI_printf("%-7s %s", "RET ", *(ShortPtr(ip))++);
		else
			PI_printf("%-7s", "RET ");
    }
	else if (opcode == 0x90) {  /* nop */
		PI_printf("%-7s", "NOP ");
    }
	else if ((opcode == 0x0f) && ((*ip & 0xff) == 0xaf)) { /* imul */
		ip++;
		w = 1;
		decode_modrm();
		PI_printf("%-7s %s, %s", "IMUL", REGNAMES(reg), EA());
	}
	else if ((opcode & 0xfd) == 0x69) { /* imul */
		char *Gv;
		char *Ev;
		long Ibv;
		w = 1;
		decode_modrm();
		Gv = REGNAMES(reg);
		Ev = EA();
		if ((opcode & 0x02) == 0)
			{ w = 1; Ibv = decode_immedval(); }
		else
			{ w = 0; Ibv = decode_immedval(); }
		PI_printf("%-7s %s, %s, %lx", "IMUL", Gv, Ev, Ibv);
	}
    else
	   return(0);

	return(1);
}



