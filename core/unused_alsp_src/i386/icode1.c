/*
 * icode1.c			-- clause code generator for 80386
 *
 *	Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 *
 * Authors: Keith Hughes, Ilyas Cicekli, Kevin A. Buettner
 * Creation: 2/12/87
 * Revision History:
 *	Revised: 12/31/90,	Ilyas,		Relocation information added
 *	Revised: 12/30/92,	kev,		put Keith's split up files back
 *						into one common one
 *
 */


#include <stdio.h>

#include "config.h"
#include "types.h"
#include "coerce.h"
#include "alloc.h"
#include "parser.h"
#include "mtypes.h"
#include "icom.h"
#include "module.h"
#include "wamregs.h"
#include "machinst.h"
#include "compile.h"
#include "varproc.h"
#include "icodegen.h"
#include "icode.h"
#include "istr.h"
#include "rinfo.h"
#include "wintcode.h"
#include "module.h"
#include "tokens.h"
#include "winter.h"		/* added by kev, 12/29/92 */
#include "fatal.h"		/* added by kev, 12/30/92 */

/*
 * system_debugging is a flag which indicates whether we
 * are doing low level debugging or not.  In this file,
 * this flag controls whether or not the icode instructions
 * are printed out as they are emitted.
 */

extern system_debugging;


/*
 * READMODE, WRITEMODE, CAPTUREMODE, MACROMODE, and ALLOCMODE
 * are the potential values which the variable capturemode may
 * take on.
 */

#define	READMODE	0	/* in read mode					*/
#define WRITEMODE	1	/* in write mode (the default)	*/
#define CAPTUREMODE	2	/* capturing unify instrs for	*/
						/* read/write mode expansion	*/
#define MACROMODE	3	/* loading a macro expansion	*/
#define	ALLOCMODE	4	/* emitting allocation code		*/



/*
 * Number of maximum calls in a clause.  This value will
 * depend on the value of MAXGLS from varproc.h.
 */

#define MAXCALLS 	MAXGLS		/* from varproc.h */

/*
 * Initial capture area size
 * Initial icode buffer size
 */
#define CAPTURESIZE 	0x0100

#define MIN_ICBUFSIZE 	0x010000 		/* 64K bytes */
#define MAX_ICBUFSIZE	0x100000		/* 1M  bytes */
#define ICBUFSAFETY		0x0100			/* 256 bytes */


/* 
 * Where to go for a determinate start of clause 
 */
Code *dstart;		


/*
 * Mask corresponding to top of stack of the locations to save when creating
 * an environment. This value is also used to determine whether a clause 
 * requires the unit clause variations of the retry and trust code.
 */
static long envsavemask;


/*
 * Are we creating .obp file?
 */
int makeobp;


/* 
 * Data structure for storing gc call information 
 */
static struct callinfostruct {
	Code *patchaddr;
	long  	argenvsize;
	long 	argmask;
} callinfo[MAXCALLS];

static int callidx;			/* call index			*/


/*
 * Capture mode
 */
int capturemode = WRITEMODE;


/*
 * Capture area
 */
static struct capturestruct {
	int iidx;
	int w,x,y,z;
} initial_capturearea[CAPTURESIZE];

static struct capturestruct *capturearea = initial_capturearea;

static unsigned long capturearea_size = CAPTURESIZE;

static int captureidx;

Code *capturepatch1;		/* patch indices for read/write mode */
Code *capturepatch2;
Code *capturepatch3;


/*
 * Macro area
 */
static struct capturestruct initial_macroarea[CAPTURESIZE];

static struct capturestruct *macroarea = initial_macroarea;

static unsigned long macroarea_size = CAPTURESIZE;

static int macroidx;

Code *ic_macropatch1;
Code *ic_macropatch2;


/*
 * Allocation area
 */
static struct capturestruct initial_allocarea[CAPTURESIZE];

static struct capturestruct *allocarea = initial_allocarea;

static unsigned long allocarea_size = CAPTURESIZE;

static int allocidx;


static increase_a_capture_area(area,area_size)
	struct capturestruct **area;
	unsigned long 		 *area_size;
{
	struct capturestruct *new_area;
	unsigned long 		 new_area_size;
	unsigned long 		 new_szinbytes;
	unsigned long 		 szinbytes;

	new_area_size = *area_size * 2;
	new_szinbytes = new_area_size * sizeof(struct capturestruct);
	new_area =  (struct capturestruct *) malloc(new_szinbytes);
	if (new_area == 0 )
	  fatal_error(FE_CAPTURE_INC,0);

	/* Copy  areas into their new locations */
	szinbytes = *area_size * sizeof(struct capturestruct);
	memcpy((void *)new_area, (void *)*area, szinbytes);

	/* Free current  areas if they are not initial ones   */
	if (*area_size != CAPTURESIZE)
		free((void *)*area);

	/* Now, new  areas are current  arease */
	*area = new_area;
	*area_size = new_area_size;
}




/* 
 * 1 if the first argument has been processed.  0 otherwise. 
 */
int firstargprocessed;


/* 
 * Pointer to the determinate code for the first argument in which
 * the dereference code is skipped because A1 has the dereferenced
 * first argument. 
 */
Code *firstargptr;
				

/* 
 * Icode Buffer
 */
static Code initial_icode_buf[MIN_ICBUFSIZE];

Code *icode_buf;

Code *ic_ptr;

unsigned long icode_buf_size = MIN_ICBUFSIZE; 

static Code *icode_buf_end;


init_icode_buf()
{
	Code *new_icode_buf;

	if (icode_buf_size < MIN_ICBUFSIZE)
		icode_buf_size = MIN_ICBUFSIZE;
	else if (icode_buf_size > MAX_ICBUFSIZE)
		icode_buf_size = MAX_ICBUFSIZE;

	if (icode_buf_size == MIN_ICBUFSIZE) 
		icode_buf = (Code *) (&initial_icode_buf[0]);
	else {
		if ((icode_buf = (Code *) malloc(icode_buf_size)) == 0 ) {
		  return(0);
		}
	}

	icode_buf_end = icode_buf + (icode_buf_size - ICBUFSAFETY);
	return( 1 );
}



#define ICBUF_OVERFLOW_CHECK 										\
	if (ic_ptr > icode_buf_end)  									\
		fatal_error(FE_ICODEBUFOVER,0);

#define ICBUF_OVERFLOW_CHECK1(sz)									\
	if ((ic_ptr+(sz)) > icode_buf_end) 							\
		fatal_error(FE_ICODEBUFOVER,0);



/*
 * Base register numbers
 */
int ic_base_nums[] = BASEREGS;




/* The following was in imisc.c */


extern wm_docut();
long DoCutPtr = (long)(wm_docut);


/* 
 * ic_put should be able to put down one opcode 
 */

ic_put(data)
	int data;
{
	*ic_ptr++ = (Code)data;
}


ic_putl(data)
	long data;
{
	*(LongPtr(ic_ptr))++ = data;
}


icPutAddrMode(reg,rm,disp)
	int reg, rm;
	Offset disp;
{
	if (rm == NOBASE) {		/* No base register. Is 32-bit */
		ic_put((reg << 3) | 0x5);
		ic_putl(disp);
	} else if (disp == 0) {
		if (rm == 5) {		/* Can't do BP as [BP] */
			ic_put(0x45 | (reg << 3));
			ic_put(0);
		} else {
			ic_put((reg << 3) | rm);	/* No displacement */
		}
		if (rm == 4)
			ic_put(0x24);
	} else if ((disp < -127) || (disp > 127)) { /* 32-bit displacement */
		ic_put(0x80 | (reg << 3) | rm);
		if (rm == 4) 		/* escape code */
			ic_put(0x24);	/* No scale, nor index. Just base */
		ic_putl(disp);
	} else {		/* 8-bit displacement */
		ic_put(0x40 | (reg << 3) | rm);
		if (rm == 4) 		/* escape code */
			ic_put(0x24);
		ic_put(disp);
	}
}


/*
 * ic_backpatch is used to fix up the byte displacement of a JMP or Jcc
 * 	instruction.  If using a byte displacement can't be used because the
 * 	forward reference is too far away, the instructions below the branch
 * 	instruction are shifted forward in memory by one word and a word 
 * 	displacement is installed.  The single parameter where is the index 
 * 	of the word after the branch instruction.  This is the location to 
 * 	install the word displacement, if necessary.  Fixing up a byte 
 * 	displacement involves backing up one word and oring in the displacement.
 * 	Note that we assume that the byte displacment starts out as zero which 
 * 	is necessary for either the short fixup or the word fixup.
 */
ic_backpatch(where,conditional)
	Code *where;
	int conditional;
{
	int disp = (ic_ptr-where)*CodeSize;

	if (conditional) {
		if (disp <= 127) {
			*(where-1) |= disp;	/* or in byte displacement */
		}
		else {	/* need to shift and make room for word displacement */
			Code *ip;

			for (ip=ic_ptr; ip >= where; ip--)
				*(ip+1) = *ip;
			ic_ptr++;
			*where = disp+2;
		}
	} else {	/* non-conditional branch */
	}
}


/*
 * ic_deref1 is used to generate a dereference loop for arguments which we
 * 	assume are not self-referential (they may be, but then the loop will do
 * 	extra work). The pp parameter is the patch pointer.  This value is
 *	passed into ic_drf1fix which will fill in the forward read mode label
 *	appropriately for this dereference sequence.
 *
 */
ic_deref1(n,pp,pp1)
	int n;		/* number of address register to use in dereferencing */
	Code **pp;		/* patch pointer */
	Code **pp1;		/* patch pointer */
{
	MOVRR(EBX,EAX);			/* mov ebx,eax */

	ic_put(0x24); ic_put(MTP_TAGMASK); 	/* and al,MTP_TAGMASK */

	JNE(0);					/* jne readBranch */
	LABEL(*pp)

	MOVRM(EAX,EBX,0);		/* mov eax,[ebx] */
	CMPRR(EBX,EAX);			/* cmp ebx,eax */
	JNE(-12);
	LABEL(*pp1)
}


/*
 * ic_drf1fix patches the bne instructions generated by ic_deref1.  The
 *	first bne instruction, it patches by putting in the displacment
 *	to the current location.  The second bne is left alone if a byte
 *	displacment is possible.  Otherwise it is backed up in order to get
 *	back to the proper point.
 *
 * where0 - position of displacement for first bne
 * where1 - where the first bne should jump to
 * where2 - position of displacement of second bne
 */
ic_drf1fix(where0,where1,where2)
	Code *where0, *where1, *where2;
{
	int disp = (where1-where0)*CodeSize;

	/* If small displacement, no work to do */
	if (disp <= 127)
		*(where0-1) |= disp;
	else {		/* Sigh. Effort is involved. */
		Code *ip;

		/* Move code up to ic_ptr by sizeof(BigOffset) bytes */
		for (ip=ic_ptr-1; ip>=where0; ip--)
			*(ip+sizeof(BigOffset)) = *ip;

		RELOC_UPDATE(where0, sizeof(BigOffset))

		ic_ptr += sizeof(BigOffset);

		/* Fix the first JNE by putting in a large displacement */
		*(where0 - 2) = (Code)(0x0f);
		*(where0 - 1) = (Code)(0x85);
		*((BigOffset *)where0) = (BigOffset)disp;

		/* Patch up bottom JNE (which has been moved by
			sizeof(BigOffset)) bytes
		*/

		*(where2 + sizeof(BigOffset) - 1) -= sizeof(BigOffset);
	}
}


/*
 * Instruction:	ic_addtosp
 * Function:	adds a number to the stack pointer
 * Parameters:	
 * 		num	-- number to add
 * Code summary:
 *		lea	sp,[sp+size]
 */
ic_addtosp(num,x,y,z)
	int num,x,y,z;
{
	if (num != 0) 
		LEA(SP_REG,SP_REG,sizeof(PWord)*num)
}


/*
 * ic_uiastr(s)
 * 
 * Lay down what is needed to create the UIA represented by the string s.
 * This is closely tied to what is found in uia.m4.
 */
ic_uiastr(s)
	char *s;
{
	register int l,i;

	/* Calculate length in PWords (including null byte) and round up */
	i = strlen(s)+1;
	l = i / sizeof(PWord);
	/* If remainder is non-zero, got extra word going on */
	if ((i %= sizeof(PWord))) {
		i = sizeof(PWord) - i;
		l++;
	}
   
	l++;		/* Include a fence */

	ic_putl((PWord)(l+1));		/* number of PWords to follow */

	ic_putl(MMK_FENCE(l));		

	/* put down the string. Had to be done this way so that OBP files
	   worked correctly on the 386i */
	do {
		ic_put(*s);
	} while (*s++);
	while (i--)
		ic_put((char)0);

	ic_putl(MMK_FENCE(l));
}


/*
 * Instruction: ic_move
 * Function:	emits a move instruction
 * Parameters:	
 * 		sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary
 *		move.l	SourceEA,DestEA
 */
ic_move(sbase,sdisp,dbase,ddisp)
	int sbase,sdisp,dbase,ddisp;
{
	MOVRM(EAX,ic_base_nums[sbase],sizeof(PWord)*sdisp);
	MOVMR(ic_base_nums[dbase],sizeof(PWord)*ddisp,EAX);
}


/*-----------------*/
/* ienvctl.c */



/*
 * Instruction:	ic_call
 * Function:	implements the procedure call
 * Parameters:	
 * 		p	-- token index of procedure to call
 *		a	-- arity of procedure
 * Code summary:
 *		call	p/a
 */
ic_call(p,a,y,z)
	int p,a,y,z;
{
	ntbl_entry *ent;

	ent = w_nameentry((PWord)cur_mod,(PWord)p,a);
	CALL_RELOC((long)ent->call_entry,RELOC_PROC_CALL,0)
}


/*
 * Instruction: ic_execute
 * Function:	implements the execute instruction (call of last goal)
 * Parameters:	
 * 		p	-- token index of procedure to execute
 *		a	-- arity of procedure
 * Code summary:
 *		jmp	p/a
 */
ic_execute(p,a,x,y)
	int p,a,x,y;
{
	ntbl_entry *ent;

	ent = w_nameentry((PWord)cur_mod,(PWord)p,a);
	MOVRI_RELOC(EAX,(long)ent->exec_entry,RELOC_PROC_EXEC,0)

	JMPR(EAX)
}


/*
 * Instruction: ic_allocate
 * Function:	allocates and space for first call in a multi-goal clause.
 * Parameters:	
 * 		size	-- size of arguments (in longwords)
 * Code Summary:
 *		lea	SP,[SP-size]
 */
ic_allocate(size,x,y,z)
	int size,x,y,z;
{
	ic_addtosp(-size);
}


/*
 * Instruction: ic_allocate1
 * Function:	allocates space for goal in a clause with one goal
 * Parameters:	
 * 		size	-- space on stack needed in non-determinate case
 *	  			   (this is usually equal to the size of the head)
 * Code Summary:
 *		move.l	SP, A
 *		cmp	SP, SPB
 *		ja	1f
 *		lea	SP,[SP-size*4]
 * Note:
 *	We expect ic_endalloc1 to patch the ja address.  Between
 *	ic_allocate1 and ic_endalloc1, the compiler will emit move instructions
 *	which will set up the variables which are the same for the
 *	non-determinate case.
 *
 *	The test for capturemode being unequal to READMODE is used for
 *	patching in the determinate code for the first argument.  When
 *	doing the normal allocate, the capturemode will be WRITEMODE so all
 *	of the allocation code will be used.  But when the first argument is
 *	being processed, we can omit the code for the non-determinate cases.
 */
ic_allocate1(size,x,y)
	int size,x,y;
{
	if (capturemode != READMODE)
		ic_addtosp(-size,0,0,0);
}


/*
 * Instruction:	ic_endallocate1
 * Function:	If needed, we set down where to jump into the clause in the
 *				case of a determinate entry.
 * Parameters:	none
 */
ic_endallocate1(w,x,y,z)
	int w,x,y,z;
{
	if (capturemode != READMODE)
		LABEL(dstart)
}


/*
 * Instruction: ic_deallocate1 through ic_deallocate4
 * Description:	These four functions perform the same actions as ic_deallocate.
 */

static Code *deallocate2patch;
static Code *deallocate3patch;


ic_deallocate1(w,x,y,z)
	int w,x,y,z;
{
	deallocate3patch = (Code *) 0;
}


ic_deallocate2(size1,x,y,z)
	int size1,x,y,z;
{
	CMPRR(SPB_REG,E_REG)
	JA(0)
	LABEL(deallocate2patch)
	LEA(SP_REG,SPB_REG,-sizeof(PWord)*size1)
}


ic_deallocate3(w,x,y,z)
	int w,x,y,z;
{
	Offset disp;

	JMP(0)
	LABEL(deallocate3patch)

	disp = OFFSET(deallocate2patch);

	if (disp <= 127) {
		PATCHDISP(deallocate2patch)	/* Fix displacement */
	} else {	/* need to shift and make room for Code *displacement */
		Code *ip;

		for (ip=(ic_ptr-1); ip >= deallocate2patch; ip--)
			*(ip+sizeof(BigOffset)) = *ip;

		RELOC_UPDATE(deallocate2patch, sizeof(BigOffset))

		ic_ptr += sizeof(BigOffset);

		/* Lay down new instructions above */
		ip = ic_ptr;

		/* Had a JA(0) (short form) which is 2 Code words long */
		MOVETOLABEL(deallocate2patch-2)

		/* New instruction is long form */
		JAL(disp)

		/* Put icode pointer back */		
		ic_ptr = ip;

		SHIFTLABEL(deallocate2patch,sizeof(BigOffset))
		SHIFTLABEL(deallocate3patch,sizeof(BigOffset))
	}
}


ic_deallocate4(size2,x,y,z)
	int size2,x,y,z;
{
	LEA(SP_REG,E_REG,-sizeof(PWord)*size2)

	if (deallocate3patch)
		PATCHLDISP(deallocate3patch)
}


/*
 * Instruction:	ic_trim
 * Function:	Sets up stack pointer for next call in a multi-goal clause
 *				This code is not used before the first or last goals.  It
 *				also trims some of the environment away when possible.
 * Parameters:	
 * 		size1	-- value in longwords of environment that must
 *	   			   persist after the call.
 *		size2	-- value in longwords of size of next goal or
 *	   			   difference in size of environment previously
 *	   			   and current size which ever is greater
 *		isdeterminateforsure 	
 *				-- 1 if the compiler knows for certain that the clause
 *				   is determinate at this point; 0 otherwise
 * Code Summary:
 *	Determinate case:
 *		lea	-(size1+size2)*4(E), SP
 *	Unknown case:
 *		move.l	SPB, A0
 *		cmp	E, SPB
 *		blo	1f
 *		lea	-size1*4(E), A0
 * 	1:	lea	-size2*4(A0), SP
 */
ic_trim(size1,size2,isdeterminateforsure,x)
	int size1,size2,isdeterminateforsure,x;
{
	Code *p1;

	if (isdeterminateforsure) {
		LEA(SP_REG,E_REG,-sizeof(PWord)*(size1+size2))
	} else {
		MOVRR(EAX,SPB_REG)
		CMPRR(SPB_REG,E_REG)
		JB(0)
		LABEL(p1)
		LEA(EAX,E_REG,-sizeof(PWord)*size1)
		PATCHDISP(p1)
		LEA(SP_REG,EAX,-sizeof(PWord)*size2)
	}
}


/*
 * Instruction: ic_proceed
 * Function:	Implements the return from procedure
 * Parameters:	
 * 		base	-- base from which to get the return address
 *		disp	-- displacement off of base of return address
 *	   			   (in longwords)
 * Code Summary:
 *		jmp	[base+disp]
 */
ic_proceed(base,disp,y,z)
	int base,disp,y,z;
{
	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
	MOVRM(E_REG,ic_base_nums[base],sizeof(PWord)*(disp-1))
	JMPR(EAX)
}


/*
 * Instruction:	ic_inline_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *				usually is emitted as a result of a macro as the last goal.
 *
 * Parameters:	none
 * Code Summary:
 *		move.l	(E), A0
 *		move.l	-4(E), E
 *		jmp	(A0)
 */
ic_inline_proceed()
{
	MOVRM(EAX,E_REG,4)	/* Get return address */
	MOVRM(E_REG,E_REG,0)	/* Get old environment */
	JMPR(EAX)
}


/*
 * Instruction: ic_init_yvar1
 * Function:	Initializes an environment variable.  All environment variables
 *	        	must be initialized prior to the first call for garbage
 *	        	collection purposes.
 * Parameters:  
 * 		ebase	-- base register needed to access environment
 *		edisp	-- displacement for accessing the variable
 *
 * Code Summary:
 *		lea	edisp(ebase), A0
 *		move.l	A0, (A0)+
 */
ic_init_yvar1(ebase,edisp,x,y)
	int ebase,edisp,x,y;
{
	LEA(EAX,ic_base_nums[ebase],sizeof(PWord)*edisp)
	MOVMR(EAX,0,EAX)
	LEA(EAX,EAX,sizeof(PWord))
}


/*
 * Instruction: ic_init_yvar2
 * Function:	Called after ic_init_yvar1 to efficiently initialize other
 *				environment variables.
 * Parameters:	
 * 		incr	-- number when multiplied by 4 needed to add to A0
 *				   for proper address. Note that when the environment variables
 *				   go sequentially, incr will be zero due to the use of the
 *				   post increment on the move.
 *
 * Code Summary:
 *		adda.w	#incr*4, A0	(when incr > 2)
 *	or
 *		addq.l	#incr*4, A0	(when incr == 1 or incr == 2)
 *	or
 *		(nothing)		(when incr == 0)
 *
 *		move.l	A0, (A0)+
 */
ic_init_yvar2(incr,x,y,z)
	int incr,x,y,z;
{
	incr *= sizeof(PWord);
	if (incr)
		LEA(EAX,EAX,incr)
   
	MOVMR(EAX,0,EAX)
	LEA(EAX,EAX,sizeof(PWord))
}


/*-----------------*/
/* iget.c	*/


extern long UnifyPtr;


/*
 * Instruction: ic_g_uia
 * Function:	Emits code for matching a uia in the head
 * Parameters:	
 * 		uiastr		-- string corresponding to uia
 *		base		-- index of base register
 *		disp		-- displacement from base register
 */
ic_g_uia(uiastr,base,disp,x)
	char *uiastr;
	int base,disp,x;
{
	int mode, reg;
	extern wm_g_uia();

	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
	CALL_RELOC((Code *) wm_g_uia,RELOC_GVAR,symidx_wm_g_uia);

	/* Lay down UIA after instruction */
	ic_uiastr(uiastr);
}

#ifdef PACKAGE

coff_init_sym_wm_g_uia()
{
  INSERT_SYM(symidx_wm_g_uia, "wm_g_uia");
}

#endif
/* 
 * Instruction ic_g_sym
 * Function:	Emits code corresponding to the symbol part of Warren's
 *				get_constant instruction
 * Parameters:	
 * 		tokid	-- token index
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */
ic_g_sym(tokid,base,disp,x)
	int tokid,base,disp,x;
{
	PWord con = MMK_SYM(tokid);

#ifdef AllUIAConsts
	Code *dpat, *dpat1;
	Code *l1,*l2,*l3,*l4;
 
	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
 
	ic_deref1(0,&dpat, &dpat1);	/* dereference instructions */

	/* If we get to here, we are a variable */

	MOVMI(EBX,0,con)	/* Bind Variable */ 

	CMPRR(HB_REG,EBX)	/* Do trail check */
	JB(0)
	LABEL(l1)

	CMPRR(SPB_REG,EBX)
	JA(0)
	LABEL(l2)

	LEA(TR_REG,TR_REG,-sizeof(PWord))	/* Must trail */
	MOVMR(TR_REG,0,EBX)
  	JMP(0)
	LABEL(l3) 

	if (!firstargprocessed)
		ic_1stargalloc();

	ic_drf1fix(dpat,CURRENTPOSITION,dpat1);

	/* Read mode section goes here */
	CMPRI(EBX,con)

	JE(0)
	LABEL(l4)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

	PATCHDISP(l1)
	PATCHDISP(l2)
	PATCHLDISP(l3)
	PATCHDISP(l4)
#else
	extern wm_g_sym();

	PUSHI(con)

	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)

	CALL_RELOC((Code *)wm_g_sym,RELOC_GVAR,symidx_wm_g_sym);

	/* Get rid of the constant we pushed above */
	POPR(EBX)
#endif
}

#ifdef PACKAGE

coff_init_sym_wm_g_sym()
{
  INSERT_SYM(symidx_wm_g_sym, 			"wm_g_sym");
}

#endif


/*
 * Instruction: ic_g_int
 * Function:	Emits code corresponding to the integer part of Warren's
 *				get_constant instruction
 * Parameters:	
 * 		i		-- integer to get
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */
ic_g_int(i,base,disp,x)
	int i,base,disp,x;
{
	CodePtr dpat, dpat1;
	CodePtr l1,l2,l3,l4;
	PWord con = MMK_INT(i);
 
	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
 
	ic_deref1(0,&dpat,&dpat1);	/* dereference instructions */

	/* If we get to here, we are a variable */

	MOVMI(EBX,0,con)	/* Bind Variable */ 

	CMPRR(HB_REG,EBX)	/* Do trail check */
	JB(0)
	LABEL(l1)

	CMPRR(SPB_REG,EBX)
	JA(0)
	LABEL(l2)

	LEA(TR_REG,TR_REG,-sizeof(PWord))	/* Must trail */
	MOVMR(TR_REG,0,EBX)
  	JMP(0)
	LABEL(l3) 

	if (!firstargprocessed)
		ic_1stargalloc();

	ic_drf1fix(dpat,CURRENTPOSITION,dpat1);

	/* Read mode section goes here */
	CMPRI(EBX,con)
	JE(0)
	LABEL(l4)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

	PATCHDISP(l1)
	PATCHDISP(l2)
	PATCHLDISP(l3)
	PATCHDISP(l4)
}

#ifdef PACKAGE

coff_init_sym_wm_b_reg()
{
  INSERT_SYM(symidx_wm_b_reg, 			"wm_b_reg")  ;
}

#endif

/*
 * Instruction: ic_g_value
 * Function:	emits code to unify the two operands.
 * Parameters:	
 * 		sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note: If either sbase or dbase is TREG, then the displacement is the number
 *	 of the temporary to use.
 *
 * Code Summary:
 *		move	SEA, D0
 *		move	DEA, A0
 *		jsr	unify
 */
ic_g_value(sbase,sdisp,dbase,ddisp)
	int sbase, sdisp, dbase, ddisp;
{
	MOVRM(EAX,ic_base_nums[sbase],sizeof(PWord)*sdisp)
	MOVRM(EBX,ic_base_nums[dbase],sizeof(PWord)*ddisp)

	/* See comment in front of wm_unify for why these registers
	   are saved */
	PUSHR(EDX)
	PUSHR(EBP)

	/* Call the unifier. */
	CALLI_RELOC(NOBASE,&UnifyPtr,RELOC_GVAR,symidx_UnifyPtr);

	/* Restore the registers saved above. */
	POPR(EBP)
	POPR(EDX)
}

#ifdef PACKAGE

coff_init_sym_UnifyPtr()
{
  INSERT_SYM(symidx_UnifyPtr, 			"UnifyPtr");
}

#endif

/*
 * Instruction: ic_g_list
 * Function:	emits code to unify an argument with a list
 * Parameters:	
 * 		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, then the displacement is the number of the 
 *	 temporary to use.
 *
 */
ic_g_list(base,disp,x,y)
	int base,disp,x,y;
{
	CodePtr l1,l2;

	if (capturemode == WRITEMODE) {
		/* dereference loop */
		MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
		ic_deref1(1,&capturepatch1,&capturepatch3);

		LEA(EAX,H_REG,MTP_LIST)
		MOVMR(S_REG,0,EAX)
		CMPRR(HB_REG,S_REG)
		JB(0)
		LABEL(l2)
		CMPRR(SPB_REG,S_REG)
		JA(0)
		LABEL(l1)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,S_REG)

		PATCHDISP(l1)
		PATCHDISP(l2)
	} else {
		/* sub al,MTP_LIST (short form) */
		ic_put(0x2c); ic_put(MTP_LIST);
		JE(0)
		LABEL(l1)
		FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

		if (!firstargprocessed)
			ic_1stargalloc();

		PATCHDISP(l1)
		LEA(S_REG,S_REG,-MTP_LIST)
	}
}

#ifdef PACKAGE
   /* extern wm_b_reg previously loaded into coff symbol table
      in ic_g_int()
   */
#endif


/*
 * Instruction: ic_g_structure
 * Function:	emits code to unify a structure with an argumetn
 * Parameters:	
 * 		funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, the the displacement is the number of the
 *	 temporary to use.
 */
ic_g_structure(funcid,arity,base,disp)
	int funcid,arity,base,disp;
{
	CodePtr l1,l2;
	int functor = MMK_FUNCTOR(funcid,arity);

	if (capturemode == WRITEMODE) {
		MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)

		ic_deref1(EBX,&capturepatch1,&capturepatch3);

		LEA(EAX,H_REG,MTP_STRUCT)
		MOVMR(EBX,0,EAX)

		CMPRR(HB_REG,EBX)
		JB(0)
		LABEL(l1)
		CMPRR(SPB_REG,EBX)
		JA(0)
		LABEL(l2)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,EBX)

		PATCHDISP(l1)
		PATCHDISP(l2)

		MOVMI(H_REG,0,functor)

		LEA(H_REG,H_REG,sizeof(PWord))
	} else {
		/* sub al,MTP_STRUCT (short form) */
		ic_put(0x2c); ic_put(MTP_STRUCT);
		JE(0)
		LABEL(l2)
		LABEL(l1)
		FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

		if (!firstargprocessed)
			ic_1stargalloc();

		PATCHDISP(l2)
		LEA(S_REG,S_REG,-MTP_STRUCT)
		MOVRM(EAX,S_REG,0)	/* Make into cmp [],imm at some point */
		CMPRI(EAX,functor)

		JNE(BDISP(l1))
		LEA(S_REG,S_REG,sizeof(PWord))
	}
}

#ifdef PACKAGE
   /* extern wm_b_reg previously loaded into coff symbol table
      in ic_g_int()
   */
#endif


/*-----------------*/
/* iunify.c	*/


/*
 * Instruction:	ic_u_sym
 * Function:	emits code for unify_symbol
 * Parameters:	
 * 		sym	-- symbol to unify element of structure with
 */
ic_u_sym(sym,n,y,z)
	int sym,n,y,z;
{
	PWord con = MMK_SYM(sym);
#ifdef AllUIAConsts
	CodePtr l1, p1, p2;
#else
	extern wm_u_sym();
#endif

	if (capturemode == READMODE) {
		PUSHR(S_REG)	/* Because EBX=S_REG */

		LEA(EAX,EBX,sizeof(PWord)*n)

#ifdef AllUIAConsts
		/* A deref loop if we find a variable */
		LABEL(l1)		/* to jump back to */
		MOVRM(EBX,EAX,0)
		CMPRR(EBX,EAX)
		JE(0)
		LABEL(p1)		/* to patch beq */
		MOVRR(EAX,EBX)
		ic_put(0x24);	ic_put(MTP_TAGMASK); 	/* and al,MTP_TAGMASK */
		JE(BDISP(l1))
		CMPRI(EBX,con);	
		JE(0)
		LABEL(p2)		/* to patch beq */
		FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

		PATCHDISP(p1)

		MOVMI(EAX,0,con)	/* Variable case */

		CMPRR(HB_REG,EAX)
		JB(0)
		LABEL(p1)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,EAX)

		PATCHDISP(p1)
		PATCHDISP(p2)
#else

		PUSHI(con)

		CALL_RELOC((CodePtr)wm_u_sym,RELOC_GVAR,symidx_wm_u_sym)

		/* Get con off of stack */
		POPR(EAX)
#endif

		POPR(S_REG)
	} else {
		MOVMI(H_REG,0,con)

		LEA(H_REG,H_REG,sizeof(PWord))
	}
}

#ifdef PACKAGE

coff_init_sym_wm_u_sym()
{
  INSERT_SYM(symidx_wm_u_sym,				"wm_u_sym");
}

#endif

/*
 * Instruction:	ic_u_int
 * Function:	emits code for unify_integer
 * Parameters:	
 * 		i	-- integer to unify element of structure with
 */
ic_u_int(i,n,y,z)
	int i,n,y,z;
{
	PWord con = MMK_INT(i);
	CodePtr l1, p1, p2;

	if (capturemode == READMODE) {
		PUSHR(S_REG)	/* Because EBX=S_REG */

		LEA(EBX,EBX,sizeof(PWord)*n)

		/* A deref loop if we find a variable */
		LABEL(l1)		/* to jump back to */
		MOVRM(EAX,EBX,0)
		CMPRR(EAX,EBX)
		JE(0)
		LABEL(p1)		/* to patch beq */
		MOVRR(EBX,EAX)
		ic_put(0x24);	ic_put(MTP_TAGMASK); 	/* and al,MTP_TAGMASK */
		JE(BDISP(l1))
		CMPRI(EBX,con);	
		JE(0)
		LABEL(p2)		/* to patch beq */
		FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

		PATCHDISP(p1)

		MOVMI(EAX,0,con)	/* Variable case */
		CMPRR(HB_REG,EAX)
		JB(0)
		LABEL(p1)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,EAX)

		PATCHDISP(p1)
		PATCHDISP(p2)

		POPR(S_REG)
	} else {
		MOVMI(H_REG,0,con)
		LEA(H_REG,H_REG,sizeof(PWord))
	}
}

#ifdef PACKAGE
   /* extern wm_b_reg previously loaded into coff symbol table
      in ic_g_int()
   */
#endif

/*
 * Instruction: ic_u_var
 * Function:	Implements the unify_variable instruction (for head matching)
 * Parameters:	
 * 		base 	-- Base register
 *		disp 	-- Displacement
 *		n		-- The position in the struct of this object
 * Code Summary:
 *	Read mode:
 *		mov	EAX,[S+4*n]
 *		mov	EA,EAX
 *
 *	Write mode:
 *		mov	EA,H
 *		mov	[H],H
 *		lea	H,[H+4]
 */
ic_u_var(base,disp,n,body)
	int base,disp,n,body;
{
	int reg = ic_base_nums[base];
	CodePtr l1;

	disp *= sizeof(PWord);	/* Turn into long displacement */
	n *= sizeof(PWord);		/* Turn into long displacement */

	/*
 	 * The following code generates trailing code if the destination is an
 	 * environment cell.  This is necessary for GC to work properly.
 	 *
 	 * Note:  An optimization would be to generate the trailing code only
 	 *	in the body.
 	 */
	if (body && base == E_REG) {
		LEA(EAX,reg,disp)
		reg = EAX;
		disp = 0;
		CMPRR(SPB_REG,EAX)
		JA(0)
		LABEL(l1)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,EAX)
		PATCHDISP(l1)
	}

	/*
 	 * Normal unify_variable stuff
 	 */
	if (capturemode == WRITEMODE) {
		MOVMR(reg,disp,H_REG)
		MOVMR(H_REG,0,H_REG)
		LEA(H_REG,H_REG,sizeof(PWord))
	} else {
		MOVRM(EAX,S_REG,n)
		MOVMR(reg,disp,EAX)
	}
}


/*
 * Instruction: ic_u_val
 * Function:	Implements the unify_value instruction (for head matching)
 * Parameters:	
 * 		base 	-- Base register
 *		disp 	-- Displacement
 *		n		-- The position in the struct of this object
 * Code Summary:
 *	Read mode:
 *		mov	EAX,[S+n]
 *		mov	EBX,EA
 *		jsr	unify
 *	Write mode :
 *		mov	EBX,EA
 *		mov	[H],EBX
 *		lea	H,[H+4]
 */
ic_u_val(base,disp,n,z)
	int base,disp,n,z;
{
	base = ic_base_nums[base];

	if (capturemode == READMODE) {
		MOVRM(EAX,S_REG,sizeof(PWord)*n)

		/* 
		 * Remember, we are borrowing the EBX register as an ersatz
		 * S register 
		 */
		PUSHR(EBX)
		MOVRM(EBX,base,sizeof(PWord)*((base == ESP) ? disp+1 : disp))

		/* 
		 * See comment in front of wm_unify for why these registers
		 * are saved 
		 */
		PUSHR(EDX)
		PUSHR(EBP)

		/* Call the unifier. */
		CALLI_RELOC(NOBASE,&UnifyPtr,RELOC_GVAR,symidx_UnifyPtr)
 
		/* Restore the registers saved above. */
		POPR(EBP)
		POPR(EDX)

		POPR(EBX)
	} else {
		MOVRM(EBX,base,sizeof(PWord)*disp)
		MOVMR(H_REG,0,EBX)
		LEA(H_REG,H_REG,sizeof(PWord))
	}
}

#ifdef PACKAGE
    /* extern UnifyPtr previously loaded into coff symbol table
     * in ic_g_value
     */
#endif

/*
 * Instruction: ic_u_lval
 * Function:	Implements the unify_local_value instruction
 * Parameters:
 * 		base 	-- Base register
 *		disp 	-- Displacement
 *		n		-- The position in the struct of this object
 * Code Summary:
 *	Read mode:
 *		Same as ic_u_val read mode
 *	Write mode:
 *		move.l	SrcEA, D0	; set up D0 for dereferencing
 *
 *	1:	move.l	D0, A0		; standard dereference sequence
 *		and.w	#3, D0
 *		bne.s	2f
 *		move.l	(A0), D0
 *		cmp.l	A0, D0
 *		bne.s	1b
 *
 *		cmp.l	#wm_heapbase, D0 ; see if deref'd value is on stack
 *		bhs.s	2f		; branch if in heap
 *		move.l	H, (A0)		; bind to top of heap
 *		move.l	H, A0
 *		cmp	D0, SPB		; see if need to trail
 *		bhi.s	2f		; branch if we don't need to
 *		move	D0, -(TR)	; Trail it
 *	2:	move.l	A0, (H)+	; put object on heap
 */
ic_u_lval(base,disp,n,z)
	int base,disp,n,z;
{
	if (capturemode == READMODE) {
		ic_u_val(base,disp,n,z);
	} else {
		CodePtr p1, p2, p3, p4;

		base = ic_base_nums[base];

		PUSHR(EBX)

		MOVRM(EAX,base,sizeof(PWord)*((base == ESP) ? disp+1 : disp))
		ic_deref1(EBX,&p1, &p4);

		CMPRM_RELOC(EAX,NOBASE,(CodePtr)&wm_heapbase,RELOC_GVAR,symidx_wm_heapbase)

		JAE(0)
		LABEL(p2)
		MOVMR(EBX,0,H_REG)
		MOVRR(EBX,H_REG)
		CMPRR(SPB_REG,EAX)
		JA(0)
		LABEL(p3)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,EAX)

		ic_drf1fix(p1, CURRENTPOSITION, p4);
		PATCHDISP(p2)
		PATCHDISP(p3)

		MOVMR(H_REG,0,EBX)
		LEA(H_REG,H_REG,sizeof(PWord))

		POPR(EBX)
	}
}

#ifdef PACKAGE

coff_init_sym_wm_heapbase()
{
  INSERT_SYM(symidx_wm_heapbase, 			"wm_heapbase");
}
#endif

/*
 * Instruction:	ic_u_void
 * Function:	emits instructions for handling variables in structure
 *				that occur only once in a clause
 * Parameters:	none
 * Code Summary:
 *	Read Mode: (Does nothing since we are offsetting from S)
 *
 *	Write Mode:
 *		move.l	H, (H)+
 */
ic_u_void(x,y,z,w)
	int x,y,z,w;
{
	if (capturemode == WRITEMODE) {
		MOVMR(H_REG,0,H_REG)
		LEA(H_REG,H_REG,sizeof(PWord))
	}
}


/*-----------------*/
/* iput.c	*/

extern wm_p_uia();


/*
 * Instruction: ic_p_uia
 * Function:	Emits code for putting down a uia in the body
 * Parameters:	
 * 		uiastr		-- string corresponding to the UIA
 *		base		-- index of the base register
 *		disp		-- displacement from the base register
 */
ic_p_uia(uiastr,base,disp,x)
	char *uiastr;
	int base, disp;
{
	extern wm_p_uia();

	CALL_RELOC((CodePtr) wm_p_uia,RELOC_GVAR,symidx_wm_p_uia);

	ic_uiastr(uiastr);

	MOVMR(ic_base_nums[base],sizeof(PWord)*disp,EBX)
}

#ifdef PACKAGE
coff_init_sym_wm_p_uia()
{
  INSERT_SYM(symidx_wm_p_uia, 			"wm_p_uia");
}
#endif

/*
 * Instruction: ic_p_unsafe
 * Function:	emits a put_unsafe_value instruction
 * Parameters:	
 * 		sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary
 *		move.l	SrcEA, D0	; set up D0 for dereferencing
 *
 *	1:	move.l	D0, A0		; standard dereference sequence
 *		and.w	#3, D0
 *		bne.s	2f
 *		move.l	(A0), D0
 *		cmp.l	A0, D0
 *		bne.s	1b
 *
 *		cmp.l	#wm_heapbase, D0 ; see if deref'd value is on stack
 *		bhs.s	2f		; branch if in heap
 *		move.l	H, (A0)		; bind to top of heap
 *		move.l	H, A0
 *		move.l	H, (H)+		; create variable on heap
 *		cmp	D0, SPB		; see if need to trail
 *		bhi.s	2f		; branch if we don't need to
 *		move	D0, -(TR)	; Trail it
 *	2:	move.l	A0, DstEA	; store value
 *		
 * Note: 	If either sbase or dbase is TREG, then the displacement is 
 *			the number of the temporary to use.
 * Note2: 	If the source dereferences to any stack variable, then this
 *		 	variable is bound to a heap variable.  This is somewhat different
 *		 	from wam implementations in where it is permissible to leave 
 *			variables not in the environment alone.
 */
ic_p_unsafe(sbase,sdisp,dbase,ddisp)
	int sbase,sdisp,dbase,ddisp;
{
	CodePtr p1,p2,p3, p4;

	MOVRM(EAX,ic_base_nums[sbase],sizeof(PWord)*sdisp);

	ic_deref1(EBX,&p1,&p4);

	CMPRM_RELOC(EAX,NOBASE,(CodePtr)&wm_heapbase,RELOC_GVAR,symidx_wm_heapbase);

	JAE(0)
	LABEL(p2)
	MOVMR(EBX,0,H_REG)
	MOVRR(EBX,H_REG)
	MOVMR(H_REG,0,H_REG)
	LEA(H_REG,H_REG,sizeof(PWord))
	CMPRR(SPB_REG,EAX)
	JA(0)
	LABEL(p3)
	LEA(TR_REG,TR_REG,-sizeof(PWord))
	MOVMR(TR_REG,0,EAX)

	ic_drf1fix(p1,CURRENTPOSITION,p4);
	PATCHDISP(p2)
	PATCHDISP(p3)

	MOVMR(ic_base_nums[dbase],sizeof(PWord)*ddisp,EBX)
}

#ifdef PACKAGE
    /* extern wm_heapbase already loaded into coof symbol table
     * under ic_u_lval
     */
#endif

/*
 * Instruction ic_p_int
 * Function:	Moves the prolog representation of an integer to the
 *				destination.
 * Parameters:	
 * 		i		-- integer
 *		base	-- destination base register
 *		disp	-- destination displacement
 * Code Summary:
 *		move.l	#int<i>, EA
 */
ic_p_int(i,base,disp,x)
	int i,base,disp,x;
{
	PWord con;

	con = MMK_INT(i);

	MOVMI(ic_base_nums[base],sizeof(PWord)*disp,con)
}


/*
 * Instruction ic_p_sym
 * Function:	Moves the prolog representation of a symbol to the
 *				destination.
 * Parameters:	
 * 		sym		-- symbol
 *		base	-- destination base register
 *		disp	-- destination displacement
 * Code Summary:
 *		move.l	#sym<i>, EA
 */
ic_p_sym(sym,base,disp,x)
	int sym,base,disp,x;
{
	PWord con;

	con = MMK_SYM(sym);

	MOVMI(ic_base_nums[base],sizeof(PWord)*disp,con)
}


/*
 * Instruction:	ic_p_yvar
 * Function:	Installs an unbound variable at the given environment position
 *				and puts a reference to this variable in the destination.
 * Parameters:	
 *		ebase	-- base register needed to access environment
 *		edisp	-- displacement to access variable
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary:
 *		lea	EAX,[ebase+edisp]
 *		mov	[EAX],EAX
 *		mov	EA,EAX
 * 	  	where EA is the appropriate destination formed by dbase and ddisp
 *
 * Note: If dbase is TREG then the displacement is the number of the
 *		 temporary to use.  ebase must not be TREG.
 */
ic_p_yvar(ebase,edisp,dbase,ddisp)
	int ebase,edisp,dbase,ddisp;
{
	LEA(EAX,ic_base_nums[ebase],sizeof(PWord)*edisp)
	MOVMR(EAX,0,EAX)
	MOVMR(ic_base_nums[dbase],sizeof(PWord)*ddisp,EAX)
}


/*
 * Instruction: ic_p_xvar
 * Function:	Installs an unbound variable on the top of the heap and
 *				puts a reference to this variable in the destination.
 *				The top of the heap is moved up by one location.
 * Parameters:	
 * 		base	-- base register for destination
 *		disp	-- displacement to get to destination
 * Code Summary:
 *		mov	EA,H
 *		mov	[H],H
 *		lea	H,[H+4]
 *
 * Note: If base is TREG, then the displacement indicates the number
 *		 of the temporary to use.
 */
ic_p_xvar(base,disp,x,y)
	int base,disp,x,y;
{
	MOVMR(ic_base_nums[base],sizeof(PWord)*disp,H_REG)
	MOVMR(H_REG,0,H_REG)
	LEA(H_REG,H_REG,sizeof(PWord))
}


/*
 * Instruction: ic_p_list
 * Function:	emits code to put a pointer to a list structure in an argument
 * Parameters:	
 *		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, then the displacement is the number of the 
 *		 temporary to use.
 * Code Summary:
 *		lea	EAX,[H+MTP_LIST]
 *		mov	EA,EAX
 */
ic_p_list(base,disp,x,y)
	int base,disp,x,y;
{
	LEA(EAX,H_REG,MTP_LIST)
	MOVMR(ic_base_nums[base],sizeof(PWord)*disp,EAX)
}


/*
 * Instruction: ic_p_structure
 * Function:	emits code to put pointer to a structure in an argument
 * Parameters:	
 * 		funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, the the displacement is the number of the
 *		 temporary to use.
 * Code Summary:
 *		lea	EAX,[H+MTP_STRUCT]
 *		mov	EA,EAX
 *		mov	[H],#functor
 *		lea	H,[H+4]
 */
ic_p_structure(funcid,arity,base,disp)
	int funcid,arity,base,disp;
{
	PWord func;

	func = MMK_FUNCTOR(funcid,arity);

	LEA(EAX,H_REG,MTP_STRUCT)
	MOVMR(ic_base_nums[base],sizeof(PWord)*disp,EAX)
	MOVMI(H_REG,0,func)

	LEA(H_REG,H_REG,sizeof(PWord))
}


/*
 * Instruction: ic_endstruct
 * Function:    emits code to update the heap pointer after a sequence of
 *              unify instructions for put_structure or put_list.
 * Parameters:  None.
 */
ic_endstruct(w,x,y,z)
	int w,x,y,z;
{
	/* This is a nop on the 80386 */
}


/*--------------*/
/* icut.c	*/



extern wm_docut();


/*
 * Instruction: ic_docut
 * Function:	emits code for performing the cut operation
 * Parameters:	
 * 		base	-- base register from which the environment is accessed
 *		disp	-- number of longwords to add to base to get to the environment
 *
 * Code Summary:
 *	If disp is zero:
 *		mov	EAX,basereg
 *		call	docut
 *	if disp is nonzero:
 *		lea	eax,disp[basereg]
 *		call	docut
 */
ic_docut(base,disp,z,w)
{
	if (disp == 0) {
		MOVRR(EAX,ic_base_nums[base])
	} else {
		LEA(EAX,ic_base_nums[base],sizeof(PWord)*disp)
	}

	MOVRI_RELOC(EBX,wm_docut,RELOC_GVAR,symidx_wm_docut);

	CALLR(EBX)
}

#ifdef PACKAGE
coff_init_sym_wm_docut()
{
  INSERT_SYM(symidx_wm_docut, 			"wm_docut");
}
#endif


/*
 * Instruction: ic_cut_proceed
 * Function:	Implements cut as the first, and only goal.
 * Parameters:	
 *		base	-- base from which to get the return address
 *		disp	-- displacement off of SP of return address
 *			   	   (in longwords)
 *				   These displacements also work for loading EAX for the
 *				   jump to the docut procedure.  rbase should be SP_REG.
 * Code Summary:
 *	If rdisp is nonzero or rbase is not the stack pointer:
 *		lea	SP,disp[basereg]
 *	The following two instructions are always emitted:
 *		mov	EAX,SP
 *		pop	E		% Get OldE into E
 *		jmp	docut
 *
 */
ic_cut_proceed(base,disp,y,z)
{
	base = ic_base_nums[base];

	if (base != SP_REG || disp != 0)
		LEA(SP_REG,base,sizeof(PWord)*disp)

	MOVRR(EAX,SP_REG)
	POPR(E_REG)

	MOVRI_RELOC(EBX,wm_docut,RELOC_GVAR,symidx_wm_docut);

	JMPR(EBX)
}

#ifdef PACKAGE
    /* extern wm_docut loaded into coff symbol table under
     * ic_docut
     */
#endif

/*
 * Instruction:	ic_deallocate_cut_proceed
 * Function:	Deallocates an environment and then returns.  This instruction
 *				usually is emitted as a result of a cut as the last goal.
 * Parameters:	none
 * Code Summary:
 *		mov	EAX,E
 *		mov	E,-4(E)
 *		mov	SP,EAX
 *		jmp	docut
 */
ic_deallocate_cut_proceed()
{
	MOVRR(EAX,E_REG)
	MOVRR(SP_REG,EAX)

	POPR(E_REG)

	MOVRI_RELOC(EBX,wm_docut,RELOC_GVAR,symidx_wm_docut);

	JMPR(EBX)
}

#ifdef PACKAGE
    /* extern wm_docut loaded into coff symbol table under
     * ic_docut
     */
#endif

/*
 * Instruction:	ic_cutmacro
 * Function:	emits code needed for performing cuts within a cut macro
 * Parameters:	
 * 		ebase	-- environment base register
 *		edisp	-- displacement to get to end of arguments
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Code Summary:
 *		lea	eax, 4*edisp[ebase]
 *		subi	eax, $wm_heapbase
 *		shl	eax, $3
 *		addq	#3, D0
 *		mov	EA, eax
 *
 * Note:
 *	The idea here is to put a relatively small integer down which
 *	will represent the cutpt.  The real cutpt may be found by loading
 *	this location and shifting right by MTP_CONSTSHIFT bits and masking
 *	off the lower two bits. This works since the bottom two bits are
 *	always zero, since Prolog pointers are always aligned on long word
 *	boundaries.
 */
ic_cutmacro(ebase,edisp,dbase,ddisp)
	int ebase, edisp, dbase, ddisp;
{
	MOVRM_RELOC(EAX,NOBASE,(CodePtr) &wm_heapbase,RELOC_GVAR,symidx_wm_heapbase);

	LEA(EBX,ic_base_nums[ebase],sizeof(PWord)*edisp)
	SUBRR(EAX,EBX)
	SHLR(EAX,MTP_CONSTSHIFT)
	ic_put(0x05); ic_putl(MTP_INT);		/* add eax, $MTP_INT */
	MOVMR(ic_base_nums[dbase],sizeof(PWord)*ddisp,EAX)
}

#ifdef PACKAGE
    /* extern wm_heapbase already loaded into coff symbol table
     * under ic_u_lval
     */
#endif

/*----------------*/
/* from imeta.c */

#define ICPrint(iidx,w,x,y,z) if (system_debugging) \
		printf("%s(%d,%d,%d,%d)\n", \
			(iidx >= 0 ? posic[iidx] : negic[-iidx-1]), \
			 w,x,y,z);


ic_illegal(x,y,z,w)
{
	fprintf(stderr,"\nInternal Error: Illegal icode instruction");
}


ic_start_capture(x,y,z,w)
{
	captureidx = 0;
	capturemode = CAPTUREMODE;
}


ic_begin_macro()
{
	macroidx = 0;
	capturemode = MACROMODE;
}


/*
 * End the macro capture mode. Exception code is output to see if the current
 * goal is being decompiled. If so, the regular structure building 
 * code/procedure call process is done. If not, the inline code is executed.
 * 
 * This exception code is only output if the macro code is to actually be kept.
 * It may not be used if some condition takes place during compilation that 
 * says that only the procedure call method should be used. Examples of this 
 * are void variables in an arithmetic statement, which should generate a fail
 * right away, but we have to be able to decompile the silly thing.
 */
ic_end_macro(keepit)
	int keepit;
{
	capturemode = WRITEMODE;

	/* Only output the interrupt check if there is macro code put down */
	if (keepit) {
		MOVRR(EAX,TR_REG)
		SUBRR(EAX,H_REG)
		CMPRM_RELOC(EAX,NOBASE,&wm_safety,RELOC_GVAR,symidx_wm_safety);

		/* Only jump if no exception check. */
		JA(0) 
		LABEL(ic_macropatch1)
	}
}

#ifdef PACKAGE
coff_init_sym_wm_safety()
{
  INSERT_SYM(symidx_wm_safety, 			"wm_safety");
}
#endif


ic_callinfo(mask,nargs,envsize,w)
	int mask;
	int nargs;
	int envsize;
{
	if (!envsavemask)
		envsavemask = ((mask << 2) | 0x3) & 0x1f;

	callinfo[callidx].patchaddr = ic_ptr;
	callinfo[callidx].argenvsize = (envsize << 16) | nargs;
	callinfo[callidx].argmask = mask;
	callidx++;
	MOVRI(EAX,0)
}


#define ICODE(macro,str,addr,obp) int addr();

#include "icodedef.h"

#undef ICODE
#define ICODE(macro,str,addr,obp) {addr},

static struct {
	int (*doit)();
} instrs[] = {
#include "icodedef.h"
};


#define InMathOptimize 1


ic_put_macro(islast)
	int islast;
{
	int i;
	Offset disp;

	if (!islast) {
		JMP(0)
		LABEL(ic_macropatch2)
	}

	disp = OFFSET(ic_macropatch1);

	if (disp <= 127) {
		PATCHDISP(ic_macropatch1)	/* Fix displacement */
	} else {	/* need to shift and make room for CodePtr
			   displacement */
		Code *ip;

		for (ip=(ic_ptr-1); ip >= ic_macropatch1; ip--)
			*(ip+sizeof(BigOffset)) = *ip;

		ic_ptr += sizeof(BigOffset);

		RELOC_UPDATE(ic_macropatch1, sizeof(BigOffset))

		/* Lay down new instructions above */
		ip = ic_ptr;

		/* Had a JA(0) (short form) which is 2 Code words long */
		MOVETOLABEL(ic_macropatch1-2)

		/* New instruction is long form */
		JAL(disp)

		/* Put icode pointer back */		
		ic_ptr = ip;

		SHIFTLABEL(ic_macropatch1,sizeof(BigOffset))
		SHIFTLABEL(ic_macropatch2,sizeof(BigOffset))
		if (!islast)
			callinfo[callidx-1].patchaddr += sizeof(BigOffset);
	}

#ifdef InMathOptimize
	{
		extern int inmath_optimize;
		if ((macroidx == 5) && 
			(macroarea[0].iidx == I_MTH_INIT) &&
			(macroarea[1].iidx == I_MTH_GETINT) &&
			(macroarea[2].iidx == I_MTH_PUSHINT) &&
			((macroarea[3].iidx == I_MTH_ADD) || 
			 (macroarea[3].iidx == I_MTH_SUB)) &&
			((macroarea[4].iidx == I_MTH_GETVAL) || 
			 (macroarea[4].iidx == I_MTH_PUTINT)))
			inmath_optimize = 1;
		else
			inmath_optimize = 0;
	}
#endif
	for (i=0; i<macroidx; i++) {
		ICPrint(macroarea[i].iidx,
			macroarea[i].w,
			macroarea[i].x,
			macroarea[i].y,
			macroarea[i].z);

		(*instrs[macroarea[i].iidx].doit)(macroarea[i].w,
						  macroarea[i].x,
						  macroarea[i].y,
						  macroarea[i].z);

		ICBUF_OVERFLOW_CHECK 
	}

	if (!islast)
		PATCHLDISP(ic_macropatch2)
}


ic_end_capture(x,y,z,w)
	int x,y,z,w;
{
	int i;
	int disp1;

	capturemode = WRITEMODE;

	for (i=0; i<captureidx; i++) {
		ICPrint(capturearea[i].iidx,
			capturearea[i].w,
			capturearea[i].x,
			capturearea[i].y,
			capturearea[i].z);

		(*instrs[capturearea[i].iidx].doit)(capturearea[i].w,
					 capturearea[i].x,
					 capturearea[i].y,
					 capturearea[i].z);

		ICBUF_OVERFLOW_CHECK 
	}

	/* Need to branch around read mode code */
	JMP(0)
	LABEL(capturepatch2)

	capturemode = READMODE;
	for (i=0; i<captureidx; i++) {
		
		ICPrint(capturearea[i].iidx,
			capturearea[i].w,
			capturearea[i].x,
			capturearea[i].y,
			capturearea[i].z);

		(*instrs[capturearea[i].iidx].doit)(
			capturearea[i].w,
			capturearea[i].x,
			capturearea[i].y,
			capturearea[i].z);

		ICBUF_OVERFLOW_CHECK 

	}
	capturemode = WRITEMODE;

	/*
	 * Time to fix up the patch addresses.  capturepatch1 is the index just
	 * after the jne in the dereference code.  capturepatch2 is the index
	 * of the word just after the jmp instruction.  We would like to use a
	 * byte offset to patch the first address, but this may not always be
	 * possible.  Thus is may be necessary to do some code shifting in
	 * order to put in long displacements.
	 */
	
	/* This could possibly be a short address at times. */
	*((BigOffset *)capturepatch2-1) =
				(BigOffset)((ic_ptr-capturepatch2)*CodeSize);

	ic_drf1fix(capturepatch1,capturepatch2,capturepatch3);
}


ic_1stargalloc()
{
	int oldmode = capturemode;
	int i;

	capturemode = READMODE;

	firstargptr = ic_ptr;

	for (i=0; i<allocidx; i++) {
		ICPrint(allocarea[i].iidx,
			allocarea[i].w,
			allocarea[i].x,
			allocarea[i].y,
			allocarea[i].z);

		(*instrs[allocarea[i].iidx].doit)(allocarea[i].w,
						 allocarea[i].x,
						 allocarea[i].y,
						 allocarea[i].z);

		ICBUF_OVERFLOW_CHECK 

		if (allocarea[i].iidx == I_ALLOCATE1) {
			i++;
			while (allocarea[i].iidx != I_ENDALLOC1)
				i++;
			i--;
		}

	}
	firstargprocessed = 1;
	capturemode = oldmode;
}




icode(iidx,w,x,y,z)
	int iidx;
	int w,x,y,z;
{
	static PWord proc_id;
	static int proc_arity;
	static int firstargkey;

#ifdef OBP
	if (makeobp)
		f_icode(iidx,w,x,y,z);
#endif

	if (iidx < 0) {

		ICPrint(iidx,w,x,y,z);

		switch (iidx) {
			case IC_INIT :
				ic_ptr = icode_buf;
				callidx = 0;
				envsavemask = 0;

				capturemode = WRITEMODE;
				capturepatch1 = (Code *) 0;
				capturepatch2 = (Code *) 0;

				firstargkey = MTP_UNBOUND;
				firstargprocessed = 0;

				/* 
				 * Initialize a couple of labels to the start
				 * of the clause. These may be moved by other
				 * code, but maybe not. 
				 */
				LABEL(firstargptr)
				LABEL(dstart)
			
				/*
				 * Initialize clause relocation information
				 */
				RELOC_IC_INIT

				break;

			case IC_ENDCLAUSE :
				proc_id = w;
				proc_arity = x;

				if (firstargptr < dstart)
					firstargptr = dstart;

				{
					register int i;

					ICBUF_OVERFLOW_CHECK1(callidx*sizeof(long))

					for (i=0; i<callidx; i++) {
						/*
						 * We had put a zero down, so if this
					   	 * is not zero, the GCMAGIC has
					   	 * been moved by sizeof(BigOffset) bytes 
						 */
						if (*(callinfo[i].patchaddr+1)) {
					    	/* macro patch moved things */
							*((BigOffset *)(callinfo[i].patchaddr
									+1+sizeof(BigOffset))) =
								ic_ptr-(callinfo[i].patchaddr
									+sizeof(BigOffset));
						} else {
							/* nothing has moved */
							*((BigOffset *)(callinfo[i].patchaddr+1)) = 
								ic_ptr-callinfo[i].patchaddr;
						}
						ic_putl(callinfo[i].argenvsize);
						ic_putl(callinfo[i].argmask);
					}

					/* -1 not allowed as a valid callinfo */
					ic_putl(-1);

					/* Put down distance back to the beginning */
					ic_putl(ic_ptr-icode_buf);
				}

				break;

			case IC_ASSERTA :
				w_asserta(proc_id,proc_arity,
					icode_buf,ic_ptr-icode_buf,
					firstargkey,firstargptr-icode_buf,
					dstart-icode_buf,
					envsavemask);
				break;

			case IC_ASSERTZ :
				w_assertz(proc_id,proc_arity,
					icode_buf,ic_ptr-icode_buf,
					firstargkey,firstargptr-icode_buf,
					dstart-icode_buf,
					envsavemask);
				break;

	    case IC_ADDCLAUSE :
		  w_addclause(proc_id,proc_arity,*top_clausegroup,
					  icode_buf,ic_ptr-icode_buf,
					  firstargkey,firstargptr-icode_buf,
					  dstart-icode_buf,envsavemask);
				break;

			case IC_EXECQUERY :
				w_execquery(icode_buf,ic_ptr-icode_buf);
				break;

			case IC_EXECCOMMAND :
				w_execcommand(icode_buf,ic_ptr-icode_buf);
				break;

			case IC_ADDUSE :
				mod_adduse(cur_mod,w);
				break;

			case IC_ENDMODULE :
				end_mod();
				break;

			case IC_NEWMODULE :
				new_mod(w); /* w is token id of new module */
				break;

			case IC_EXPORTPRED :
				export_pred((PWord)cur_mod,(PWord)w,x);
				break;

			case IC_1STARG :
				switch (w) {
					case TP_VO :
						firstargkey = MTP_UNBOUND ;
						break;
					case TP_LIST :
						firstargkey = MTP_LIST;
						break;
					case TP_INT :
						firstargkey = MMK_INT(z);
						break;
					case TP_SYM :
						firstargkey = MMK_FUNCTOR(x,y);
						break;
				}
				firstargprocessed = 1;
				break;

			case IC_BEGINMACRO :
				ic_begin_macro();
				break;

			case IC_ENDMACRO :
				ic_end_macro(w);
				ICBUF_OVERFLOW_CHECK 
				break;

			case IC_PUTMACRO :
				ic_put_macro(w);
				ICBUF_OVERFLOW_CHECK 
				break;

			case IC_CREMODCLOSURE:
				createModuleClosureProcedure((PWord)w,x,(PWord)y);
				break;

			case IC_ADDTO_AUTOUSE :
				add_default_use(w);
				break;

			case IC_ADDTO_AUTONAME:
				add_default_proc((PWord)w,x);
				break;

			case IC_BEGINALLOC :
				allocidx = 0;
				capturemode = ALLOCMODE;
				break;

			case IC_ENDALLOC :
				capturemode = WRITEMODE;
				break;

			case IC_ICRESET:
				break;

			default :
				fprintf(stderr,
				 	"Warning: Unrecognized icode command (%d).\n", iidx);
				break;

		} 	/* End of Switch */

	} else if (iidx == I_END_CAPTURE) {
		ICPrint(I_END_CAPTURE,w,x,y,z);
		ic_end_capture(w,x,y,z);
		ICBUF_OVERFLOW_CHECK 

	} else if (capturemode == CAPTUREMODE) {
		if (captureidx >= capturearea_size)
			increase_a_capture_area(&capturearea,&capturearea_size);
		capturearea[captureidx].iidx = iidx;
		capturearea[captureidx].w = w;
		capturearea[captureidx].x = x;
		capturearea[captureidx].y = y;
		capturearea[captureidx].z = z;
		captureidx++;
	} else if (capturemode == MACROMODE) {
		if (macroidx >= macroarea_size)
			increase_a_capture_area(&macroarea,&macroarea_size);
		macroarea[macroidx].iidx = iidx;
		macroarea[macroidx].w = w;
		macroarea[macroidx].x = x;
		macroarea[macroidx].y = y;
		macroarea[macroidx].z = z;
		macroidx++;
	} else if (capturemode == ALLOCMODE) {
		if (allocidx >= allocarea_size)
			increase_a_capture_area(&allocarea,&allocarea_size);
		allocarea[allocidx].iidx = iidx;
		allocarea[allocidx].w = w;
		allocarea[allocidx].x = x;
		allocarea[allocidx].y = y;
		allocarea[allocidx].z = z;
		allocidx++;

		ICPrint(iidx,w,x,y,z);

		(*instrs[iidx].doit)(w,x,y,z);

		ICBUF_OVERFLOW_CHECK 

	} else {
		ICPrint(iidx,w,x,y,z);

		(*instrs[iidx].doit)(w,x,y,z);

		ICBUF_OVERFLOW_CHECK 

	}


/*	list_asm(icode_buf,ic_ptr-icode_buf);	*/

}

#ifdef PACKAGE
/*
 * initialize coff symbol table with externs needed by
 * code generated in this file
 */

coff_init_icode1()
{
  coff_init_sym_wm_g_uia();
  coff_init_sym_wm_g_sym();
  coff_init_sym_wm_b_reg();
  coff_init_sym_UnifyPtr();
  coff_init_sym_wm_u_sym();
  coff_init_sym_wm_heapbase();
  coff_init_sym_wm_p_uia();
  coff_init_sym_wm_docut();
  coff_init_sym_wm_safety();
}

#endif
